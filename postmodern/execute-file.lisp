;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN; -*-
(in-package :postmodern)

(defstruct parser
  filename
  (stream  (make-string-output-stream))
  (state   :eat)
  tags)

(defmethod print-object ((p parser) stream)
  (print-unreadable-object (p stream :type t :identity t)
    (with-slots (state tags) p
      (format stream "~a {~{~s~^ ~}}" state tags))))

(defmethod push-new-tag ((p parser))
  "Add a new element on the TAGS slot, a stack"
  (let ((tag (make-array 42
                         :fill-pointer 0
                         :adjustable t
                         :element-type 'character)))
    (push tag (parser-tags p))))

(defmethod extend-current-tag ((p parser) char)
  "The TAGS slot of the parser is a stack, maintain it properly."
  (declare (type character char))
  (assert (not (null (parser-tags p))))
  (vector-push-extend char (first (parser-tags p))))

(defmethod format-current-tag ((p parser) &optional (stream (parser-stream p)))
  "Output the current tag to the current stream."
  (format stream "$~a$" (coerce (first (parser-tags p)) 'string)))

(defmethod maybe-close-tags ((p parser) &optional (stream (parser-stream p)))
  "If the two top tags in the TAGS slot of the parser P are the
   same (compared using EQUALP), then pop them out of the stack and print
   the closing tag to STREAM."
  (when (and (< 1 (length (parser-tags p)))
             (equalp (first (parser-tags p))
                     (second (parser-tags p))))
    ;; format the tag in the stream and POP both entries
    (format-current-tag p stream)
    (pop (parser-tags p))
    (pop (parser-tags p))
    ;; and return t
    t))

(defmethod pop-current-tag ((p parser))
  "Remove current tag entry"
  (pop (parser-tags p)))

(defmethod reset-state ((p parser) &key tagp)
  "Depending on the current tags stack, set P state to either :eat or :eqt"
  (setf (parser-state p)
        (cond ((null (parser-tags p)) :eat)
              (tagp :ett)
              (t :eqt))))

#|
Here's a test case straight from the PostgreSQL docs:

(with-input-from-string (s "
create function f(text)
returns bool
language sql
as $function$
BEGIN
RETURN ($1 ~ $q$[\\t\\r\\n\\v\\\\]$q$);
END;
$function$;")
(parse-query s (make-parser)))


Another test case for the classic quotes:

(with-pgsql-connection ("pgsql:///pginstall")
(query
(with-input-from-string (s "select E'\\';' as \";\";")
(parse-query s)) :alists))

should return
(((:|;| . "';")))
|#

(defun parse-query (stream &optional (state (make-parser)))
  "Read a SQL query from STREAM, starting at whatever the current position is.

   Returns another SQL query each time it's called, or NIL when EOF is
   reached expectedly. Signal end-of-file condition when reaching EOF in the
   middle of a query.

   See the following docs for some of the parser complexity background:

   http://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-DOLLAR-QUOTING

   Parser states are:

     - EAT    reading the query
     - TAG    reading a tag that could be an embedded $x$ tag or a closing tag
     - EOT    End Of Tag
     - EQT    Eat Quoted Text
     - ETT    Eat Tag-Quoted Text
     - EDQ    Eat Double-Quoted Text (identifiers)
     - EOQ    done reading the query
     - ESC    read escaped text (with backslash)"
  (handler-case
      (loop
        :until (eq :eoq (parser-state state))
        :for char := (read-char stream)
        :do (case char
              (#\\       (case (parser-state state)
                           (:esc    (setf (parser-state state) :eqt))
                           (:eqt    (setf (parser-state state) :esc)))

               (write-char char (parser-stream state)))

              (#\'       (case (parser-state state)
                           (:eat    (setf (parser-state state) :eqt))
                           (:esc    (setf (parser-state state) :eqt))
                           (:eqt    (setf (parser-state state) :eat))
                           (:tag
                            (progn
                              ;; a tag name can't contain a single-quote
                              ;; get back to previous state
                              (let ((tag (pop-current-tag state)))
                                (format (parser-stream state) "$~a" tag))
                              (reset-state state))))

               (write-char char (parser-stream state)))

              (#\"       (case (parser-state state)
                           (:eat    (setf (parser-state state) :edq))
                           (:edq    (setf (parser-state state) :eat)))

               (write-char char (parser-stream state)))

              (#\$       (case (parser-state state)
                           (:eat    (setf (parser-state state) :tag))
                           (:ett    (setf (parser-state state) :tag))
                           (:tag    (setf (parser-state state) :eot)))

               ;; we act depending on the NEW state
               (case (parser-state state)
                 ((:eat :eqt :edq)
                  (write-char char (parser-stream state)))

                 (:tag (push-new-tag state))

                 (:eot                 ; check the tag stack
                  (cond ((= 1 (length (parser-tags state)))
                         ;; it's an opening tag, collect the text now
                         (format-current-tag state)
                         (reset-state state :tagp t))

                        (t          ; are we closing the current tag?
                         (if (maybe-close-tags state)
                             (reset-state state :tagp t)

                             ;; not the same tags, switch state back
                             ;; don't forget to add the opening tag
                             (progn
                               (format-current-tag state)
                               (setf (parser-state state) :ett))))))))

              (#\;       (case (parser-state state)
                           (:eat      (setf (parser-state state) :eoq))
                           (otherwise (write-char char (parser-stream state)))))

              (otherwise (cond ((member (parser-state state) '(:eat :eqt :ett :edq))
                                (write-char char (parser-stream state)))

                               ;; see
                               ;; http://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-STRINGS-ESCAPE
                               ;; we re-inject whatever we read in the \x
                               ;; syntax into the stream and let PostgreSQL
                               ;; be the judge of what it means.
                               ((member (parser-state state) '(:esc))
                                (write-char char (parser-stream state))
                                (setf (parser-state state) :eqt))

                               ((member (parser-state state) '(:tag))
                                ;; only letters are allowed in tags
                                (if (alpha-char-p char)
                                    (extend-current-tag state char)

                                    (progn
                                      ;; not a tag actually: remove the
                                      ;; parser-tags entry and push back its
                                      ;; contents to the main output stream
                                      (let ((tag (pop-current-tag state)))
                                        (format (parser-stream state)
                                                "$~a~c"
                                                tag
                                                char))
                                      (reset-state state)))))))
        :finally (return
                   (get-output-stream-string (parser-stream state))))
    (end-of-file (e)
      (unless (eq :eat (parser-state state))
        (error e)))))

(defstruct comment-parser
  buffer
  (stream  (make-string-output-stream))
  (state   '(:base)))

(defparameter single-line-comment-scanner
  (cl-ppcre:create-scanner "--.*"))


;;
;; If a single line comment in within a multiline comment, Postgresql will
;; ignore the single line comment.
;; comments begin with /* and end with */
;; possible states:
;; :base
;; :mlc (inside a multiline comment)
;; :mb? (maybe beginning a new multiline comment)
;; :me? (maybe ending a multiline comment)
;; :sb? (maybe beginning a single line comment)
;; :slc (inside a single line comment)
;; :sq (inside an sql quote)

(defun parse-comments (str &optional (state (make-comment-parser)))
  (loop for char across str
        do
;           (format t "~a ~a~%" char (char-code char))
           (case char
             (#\' (case (first (comment-parser-state state))
                    (:base (push :sq (comment-parser-state state))
                     (write-char #\' (comment-parser-stream state)))
                    (:mlc )
                    (:slc )
                    (:sb? (pop (comment-parser-state state))
                     (write-char #\' (comment-parser-stream state)))
                    (:mb? ; faked multi-line beginning, return to earlier state
                     (pop (comment-parser-state state))
                     (when (eq (first (comment-parser-state state))
                               :base)
                       (write-char #\/ (comment-parser-stream state))
                       (write-char #\/ (comment-parser-stream state))))
                    (:me? (pop (comment-parser-state state)))
                    (:sq (pop (comment-parser-state state))
                     (write-char #\' (comment-parser-stream state)))))
             (#\- (case (first (comment-parser-state state))
                    (:base (push :sb? (comment-parser-state state)))
                    (:mlc )
                    (:slc )
                    (:sq (write-char char (comment-parser-stream state)))
                    (:sb? (setf (first (comment-parser-state state)) :slc))
                    (:mb?  ; faked multi-line beginning, return to earlier state
                     (pop (comment-parser-state state))
                     (when (eq (first (comment-parser-state state))
                               :base)
                       (write-char #\/ (comment-parser-stream state))
                       (write-char #\/ (comment-parser-stream state))))
                    (:me? (pop (comment-parser-state state)))))
             (#\newline (case (first (comment-parser-state state))
                          (:base
                           (write-char char (comment-parser-stream state)))
                          (:mlc )
                          (:sq (write-char char (comment-parser-stream state)))
                          (:slc (pop (comment-parser-state state))
                           (write-char char (comment-parser-stream state)))
                          (:sb? (pop (comment-parser-state state))
                           (write-char char (comment-parser-stream state)))
                          (:mb?  ; faked multi-line beginning, return to earlier state
                           (pop (comment-parser-state state))
                           (when (eq (first (comment-parser-state state))
                               :base)
                             (write-char #\/ (comment-parser-stream state))
                             (write-char #\/ (comment-parser-stream state))))
                          (:me? (pop (comment-parser-state state))
                           (write-char char (comment-parser-stream state)))))
             (#\/ (case (first (comment-parser-state state))
                    (:base (push :mb? (comment-parser-state state)))
                    (:sb? (pop (comment-parser-state state)))
                    (:slc )
                    (:sq (write-char char (comment-parser-stream state)))
                    (:mb? ; faked beginning, return to earlier state (:base or :mlc)
                     (pop (comment-parser-state state))
                     (when (eq (first (comment-parser-state state))
                               :base)
                       (write-char #\/ (comment-parser-stream state))
                       (write-char #\/ (comment-parser-stream state))))
                    (:mlc  (push :mb? (comment-parser-state state))
                     )
                    (:me? ; actual ending of a multi-line comment
                         ; need to pop both the :me? amd tej :mlc
                     (pop (comment-parser-state state))
                     (pop (comment-parser-state state)))))
             (#\* (case (first (comment-parser-state state))
                    (:base (write-char char (comment-parser-stream state)))
                    (:mb?   (setf (first (comment-parser-state state)) :mlc))
                    (:mlc ; maybe starting the end of a nested multi-line comment
                     (push :me? (comment-parser-state state)))
                    (:sq (write-char char (comment-parser-stream state)))
                    (:me? ; fake ending of a multi-line comment
                     (pop (comment-parser-state state))
                     (when (eq (first (comment-parser-state state)) :mlc)
                       (push :me? (comment-parser-state state))))))
             (otherwise (case (first (comment-parser-state state))
                          (:base
                           (write-char char (comment-parser-stream state)))
                          (:mb?
                           (pop (comment-parser-state state))
                           (when (eq (first (comment-parser-state state))
                                     :base)
                             (write-char #\/ (comment-parser-stream state))
                             (write-char char (comment-parser-stream state))))
                          (:me? ; fake ending of a multi-line comment
                           (pop (comment-parser-state state)))
                          (:sb? ; fake single line comment
                           (pop (comment-parser-state state))
                           (write-char #\- (comment-parser-stream state))
                           (write-char char (comment-parser-stream state)))
                          (:sq (write-char char (comment-parser-stream state))))))
        :finally (return
                   (get-output-stream-string (comment-parser-stream state)))))


(defun remove-sql-comments (str)
  "Take a string input, replace all the multi-line comments and single line comments,
returning the resulting string."
  (parse-comments str))

(define-condition missing-i-file (error)
  ((%filename :reader filename :initarg :filename)
   (%base-filename :reader base-filename :initarg :base-filename)
   (%meta-cmd :reader meta-cmd :initarg :meta-cmd))
  (:report (lambda (condition stream)
             (format stream "We tried but failed to find file ~a at the location
specified by the ~a meta command.

Note that meta-commands \\i or  \\include in the sql file look for a file location
relative to your default pathname (current working directory), in this case:
~a.

Meta-commands \\ir or \\include_relative look for a file location relative to the
initial sql file, in this case:
~a.

 As a fallback, we also looked for it where the ~a meta command would have specified.
Can you double check that the file actually exists where it is supposed to be?"
                     (filename condition)
                     (if (eq (meta-cmd condition) 'i)
                         "\\i or \\include"
                       "\\ir or \\include_relative")
                     (uiop::get-pathname-defaults)
                     (directory-namestring (base-filename condition))
                     (if (eq (meta-cmd condition) 'i)
                         "\\ir or \\include_relative"
                       "\\i or \\include")))))

(defun line-has-includes (line)
  "Returns 'i if the first characters in a line are the postgresql include file
commands: \i or \include. Returns 'ir if the first characters in a line are postgresql
include commands \ir or \include_relative. Returns nil otherwise."
  (let  ((new-line (string-trim '(#\space #\tab) line)))
    (cond ((and (> (length new-line) 3)
                (string= "\\i " (subseq new-line 0 3)))
           (values 'i (string-trim '(#\space #\tab) (subseq new-line 3))))
          ((and (> (length new-line) 9)
                (string= "\\include " (subseq new-line 0 9)))
           (values 'i (string-trim '(#\space #\tab) (subseq new-line 9))))
          ((and (> (length new-line) 4)
                (string= "\\ir " (subseq new-line 0 4)))
           (values 'ir (string-trim '(#\space #\tab) (subseq new-line 4))))
          ((and (> (length new-line) 18)
                (string= "\\include_relative " (subseq new-line 0 18)))
           (values 'ir (string-trim '(#\space #\tab) (subseq new-line 18))))
          (t nil))))

(defun find-included-filename (meta-cmd new-filename base-filename)
  "Create full pathname if included using a \ir metacommand or \include_relative."
  (when new-filename
    (restart-case
        (let ((relative-pathname (merge-pathnames new-filename
                                                  (directory-namestring base-filename)))
              (working-pathname (merge-pathnames new-filename
                                                 (uiop::get-pathname-defaults))))
          (cond ((and (eq meta-cmd 'ir)
                      (uiop:file-exists-p relative-pathname))
                 relative-pathname)
                ((and (eq meta-cmd 'i)
                      (uiop:file-exists-p working-pathname))
                 working-pathname)
                ((and (eq meta-cmd 'ir)
                      (uiop:file-exists-p working-pathname))
                 (warn
                  "Postmodern: Using fallback to find file based on working directory position")
                 working-pathname)
                ((and (eq meta-cmd 'i)
                      (uiop:file-exists-p relative-pathname))
                 (warn
                  "Postmodern: Using fallback to find file based on relative directory position")
                 relative-pathname)
                (t (error 'missing-i-file :meta-cmd meta-cmd
                                          :filename new-filename :base-filename base-filename))))
      (use-other-values (new-full-filename)
        :report "Use a different filename location to be included."
        :interactive (lambda ()
                       (flet ((get-value ()
                                (format t "~&Enter new value for sql file to be included: ")
                                (read-line)))
                         (list (string (get-value)))))
        (find-included-filename meta-cmd new-full-filename base-filename)))))


(defun read-sql-file (filename &key (included-files nil)
                                 (output-stream (make-string-output-stream))
                                 (remove-comments t))
  "Read a given file and (default) remove the comments. Read lines from the redacted result
and return them in a stream. Recursively apply \i include instructions."
  (if (uiop:file-exists-p filename)
      (with-input-from-string
          (s (if remove-comments
                 (remove-sql-comments (alexandria:read-file-into-string filename))
                 (alexandria:read-file-into-string filename)))
        (loop
          :for line := (read-line s nil)
          :while line
          :do
             (multiple-value-bind (meta-cmd new-filename)
                 (line-has-includes line)
               (if (or (eq meta-cmd 'i)
                       (eq meta-cmd 'ir))
                   (let ((include-filename
                           (find-included-filename meta-cmd new-filename filename)))
                     (when new-filename
                       (if (not (member include-filename included-files))
                           (progn
                             (push include-filename included-files)
                             (read-sql-file include-filename :included-files included-files
                                                             :output-stream output-stream
                                                             :remove-comments remove-comments))
                           (progn
                             (warn
                              "Postmodern: Duplicate attempts to include sql files ~a skipped~%"
                              filename)
                             ""))))
                     (format output-stream "~a~%" line)))
          :finally (return output-stream)))
      (warn "Postmodern: file ~a doesn't seem to exist. If this was supposed to be an included file, please note that \\i looks for a file location relative to your default pathname, in this case ~a. \\ir looks for a file location relative to the initial included file location, in the case ~a~%"
            filename
            (uiop::get-pathname-defaults)
            (if filename
                (directory-namestring filename)
                nil))))

(defun read-queries (filename &key (remove-comments t))
  "Read SQL queries in given file and split them, returns a list. Track included
files so there is no accidental infinite loop. The default setting is to remove
sql comments from the file before executing the sql code. If that causes problems,
the remove-comments parameter can be set to nil."
  (parse-queries
   (get-output-stream-string
    (read-sql-file filename :remove-comments remove-comments))))

(defun parse-queries (file-content)
  "Read SQL queries in given string and split them, returns a list"
  (with-input-from-string (s (concatenate 'string file-content ";"))
    (let ((whitespace '(#\Space #\Tab #\Newline #\Linefeed #\Page #\Return)))
      (flet ((emptyp (query)
               (every (alexandria:rcurry #'member whitespace) query)))
        (loop :for query := (parse-query s)
              :while (and query (not (emptyp query)))
              :collect query)))))

(defun execute-file (pathname &optional (print nil) (remove-comments t))
  "This function will execute sql queries stored in a file. Each sql statement
in the file will be run independently, but if one statement fails, subsequent
query statements will not be run, but any statement prior to the failing
statement will have been commited.

Execute-file allows the sql file to include other sql files, with the
meta-commands \i or  \include which look for a file location relative to your
default pathname (current working directory) or \ir or \include_relative which
look for a file location relative to the initial sql file. If the file is not
found in the expected location, execute-file will look to see if the requested
file is in the other possible location. If that does not work, it will trigger
an error with a restart which allows you to provide a new name for the file.

If you want the standard transction treatment such that all statements succeed
or no statement succeeds, then ensure that the file starts with a begin
transaction statement and finishes with an end transaction statement. See the
test file test-execute-file-broken-transaction.sql as an example.

For debugging purposes, if the optional print parameter is set to t, format
will print the count of the query and the query to the REPL.

The default setting is to remove sql comments from the file before executing
the sql code. If that causes problems, the remove-comments parameter can be
set to nil.

IMPORTANT NOTE: This utility function assumes that the file containing the
sql queries can be trusted and bypasses the normal postmodern parameterization
of queries."
  (let ((queries (read-queries pathname :remove-comments remove-comments))
        (cnt 0))
    (dolist (query queries)
      (when print
        (incf cnt)
        (format t "~a ~a~%" cnt query))
      (postmodern:execute query))))
