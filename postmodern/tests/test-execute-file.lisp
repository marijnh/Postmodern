;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN-TESTS; -*-
(in-package :postmodern-tests)

(def-suite :postmodern-execute-file
    :description "Execute file suite for postmodern"
    :in :postmodern)

(in-suite :postmodern-execute-file)

(defparameter *good-file* (asdf:system-relative-pathname :postmodern "postmodern/tests/test-execute-file.sql"))

(defparameter *first-include-good-file* (asdf:system-relative-pathname :postmodern "postmodern/tests/test-first-include-execute-file.sql"))

(defparameter *fail-include-file* (asdf:system-relative-pathname :postmodern "postmodern/tests/test-fail-include-execute-file.sql"))

(defparameter *bad-file* (asdf:system-relative-pathname :postmodern "postmodern/tests/test-execute-file-broken.sql"))
(defparameter *bad-file-with-transaction* (asdf:system-relative-pathname :postmodern "postmodern/tests/test-execute-file-broken-transaction.sql"))

;; Test Parse Dollar Quoted String Constants
(test basic-dollar-quote
  (is (equal (with-input-from-string (s "$$Dianne's horse$$;;")
               (pomo::parse-query s))
             "$$Dianne's horse$$"))
  (is (equal (with-input-from-string (s "SELECT $$ UPPER(';'); $$;")
            (pomo::parse-query s))
             "SELECT $$ UPPER(';'); $$"))
  (is (equal (with-input-from-string (s "DO $$
DECLARE
  sql text;
  dropped int;
BEGIN
  SELECT count(*)::int, 'DROP FUNCTION ' || string_agg(oid::regprocedure::text, '; DROP FUNCTION ')
  FROM pg_proc
    WHERE proname ='gateway_error' AND pg_function_is_visible(oid)
  INTO dropped, sql;
  IF dropped > 0 THEN
    EXECUTE sql;
  END IF;
END;
$$ LANGUAGE plpgsql;")
               (pomo::parse-query s))
"DO $$
DECLARE
  sql text;
  dropped int;
BEGIN
  SELECT count(*)::int, 'DROP FUNCTION ' || string_agg(oid::regprocedure::text, '; DROP FUNCTION ')
  FROM pg_proc
    WHERE proname ='gateway_error' AND pg_function_is_visible(oid)
  INTO dropped, sql;
  IF dropped > 0 THEN
    EXECUTE sql;
  END IF;
END;
$$ LANGUAGE plpgsql")))

(test dollar-quote-with-matching-tags
  (is (equal (with-input-from-string (s "$a$Dianne's horse$a$;;")
               (pomo::parse-query s))
             "$a$Dianne's horse$a$"))
  (is (equal (with-input-from-string (s "$a$Dianne's horse$a$ ;;")
               (pomo::parse-query s))
             "$a$Dianne's horse$a$ "))
  (is (equal (with-input-from-string (s "$a $Dianne's horse$a $ ;;")
               (pomo::parse-query s))
             "$a $Dianne's horse$a $ "))
  (is (equal (with-input-from-string (s "$abc$Dianne's horse$abc$;;")
               (pomo::parse-query s))
             "$abc$Dianne's horse$abc$"))
  (is (equal (with-input-from-string (s "$_$Dianne's horse$_$;;")
                    (pomo::parse-query s))
             "$_$Dianne's horse$_$"))
  (is (equal (with-input-from-string (s "$.$Dianne's horse$.$;;")
                    (pomo::parse-query s))
             "$.$Dianne's horse$.$"))
  (is (equal (with-input-from-string (s "BEGIN
    RETURN ($1 ~ $q$[\t\r\n\v\\]$q$);
END;;")
               (pomo::parse-query s))
             "BEGIN
    RETURN ($1 ~ $q$[\t\r\n\v\\]$q$)")))

(test dollar-quote-with-mismatched-tags
  (signals error (with-input-from-string (s "$a$Dianne's horse$b$;;")
        (pomo::parse-query s)))
  (signals error (with-input-from-string (s "$a$Dianne's horse$$;;")
        (pomo::parse-query s)))
  (signals error (with-input-from-string (s "$$Dianne's horse$b$;;")
        (pomo::parse-query s))))

(test dollar-quote-with-parameters-outside-tag
  (is (equal (with-input-from-string (s "$abc$Dianne's horse$abc$ where id= $1;;")
               (pomo::parse-query s))
             "$abc$Dianne's horse$abc$ where id= $1"))
  (is (equal (with-input-from-string (s "$abc$Dianne's horse$abc$ where id= $1 and name=$2;;")
               (pomo::parse-query s))
             "$abc$Dianne's horse$abc$ where id= $1 and name=$2"))
  (is (equal (with-input-from-string (s "$abc$Dianne's horse$abc$ where id= $1 ;;")
               (pomo::parse-query s))
             "$abc$Dianne's horse$abc$ where id= $1 "))
  (is (equal (with-input-from-string (s "$abc$Dianne's horse$abc$ where id= $1 and name=$2;; ")
               (pomo::parse-query s))
             "$abc$Dianne's horse$abc$ where id= $1 and name=$2")))

(test dollar-quote-with-parameters-inside-tag
  (is (equal (with-input-from-string (s "s $_$ ab = $1 cd $_$ s;;")
               (pomo::parse-query s))
             "s $_$ ab = $1 cd $_$ s")))

(test dollar-quote-with-internal-statements
  (is (equal (with-input-from-string (s "CREATE FUNCTION public.film_in_stock(p_film_id integer, p_store_id integer, OUT p_film_count integer) RETURNS SETOF integer
    LANGUAGE sql
    AS $_$
     SELECT inventory_id
     FROM inventory
     WHERE film_id = $1
     AND store_id = $2
     AND inventory_in_stock(inventory_id);
$_$;")
               (pomo::parse-query s))
             "CREATE FUNCTION public.film_in_stock(p_film_id integer, p_store_id integer, OUT p_film_count integer) RETURNS SETOF integer
    LANGUAGE sql
    AS $_$
     SELECT inventory_id
     FROM inventory
     WHERE film_id = $1
     AND store_id = $2
     AND inventory_in_stock(inventory_id);
$_$")))

(test dollar-quote-with-internal-parameters
  (is (equal (with-input-from-string (s "$abc$Dianne's $1 horse$abc$;;")
               (pomo::parse-query s))
             "$abc$Dianne's $1 horse$abc$")))

(test dollar-quote-with-nested-tags
  (is (equal (with-input-from-string (s "$function$
BEGIN
    RETURN ($1 ~ $q$ something here $q$);
END;
$function$;")
               (pomo::parse-query s))
"$function$
BEGIN
    RETURN ($1 ~ $q$ something here $q$);
END;
$function$")))

(test dollar-quote-with-digit-tag
  (is (equal (with-input-from-string (s "$a1$Dianne's $1 horse$a1$;;")
               (pomo::parse-query s))
             "$a1$Dianne's $1 horse$a1$"))
  (is (equal (with-input-from-string (s "$a12$Dianne's $1 horse$a12$;;")
               (pomo::parse-query s))
             "$a12$Dianne's $1 horse$a12$"))
  (is (equal (with-input-from-string (s "$a1$Dianne's $1 horse$a1$ ;;")
               (pomo::parse-query s))
             "$a1$Dianne's $1 horse$a1$ "))
  (signals error (with-input-from-string (s "$1ab$Dianne's $1 horse$1ab$ ;;")
                   (pomo::parse-query s))))

;; Test Parse Comments

(test basic-multi-line1
  (is (equal (pomo::parse-comments " something1 /* comment */ something2")
             " something1  something2")))

(test basic-multi-line2
  (is (equal (postmodern::parse-comments " something1 /*
  comment */ something2")
             " something1  something2")))

(test basic-single-line
  (is (equal (postmodern::parse-comments " something1 -- comment */ something2")
             " something1 ")))

(test multi-line-within-single-line
  (is (equal (postmodern::parse-comments " something1 -- /* comment */ something2")
             " something1 ")))

(test multi-line-within-multi-line
  (is (equal (postmodern::parse-comments " something1 /* outside comment
 /* inside comment */ bad-something2 */ something2")
             " something1  something2")))

(test broken-nested-muli-line-comments
  (is (equal (pomo::parse-comments "/* comment /* still the same comment */")
             "")))

(test single-line-within-multi-line
  (is (equal (postmodern::parse-comments " something1 /* comm -- ent */ something2")
             " something1  something2")))

(test basic-fake-single-line
  (is (equal (postmodern::parse-comments " something1 - something2")
             " something1 - something2")))

(test basic-fake-muli-line
  (is (equal (postmodern::parse-comments " something1 / something2")
             " something1 / something2")))

(test multi-line-within-sql-string1
  (is (equal (postmodern::parse-comments " something ('my wonder /* something */ company ')")
             " something ('my wonder /* something */ company ')")))

(test multi-line-within-sql-string2
  (is (equal (postmodern::parse-comments "insert into a (d) values ('/*');")
             "insert into a (d) values ('/*');")))

(test single-line-within-sql-string1
  (is (equal (postmodern::parse-comments " something ('my wonder -- company ')")
             " something ('my wonder -- company ')")))

(test single-line-within-sql-string2
  (is (equal (postmodern::parse-comments "insert into a (d) values ('-- /*');")
             "insert into a (d) values ('-- /*');")))

(test asterisk-no-comment
  (is (equal (pomo::parse-comments "select * from x")
             "select * from x")))

(test unicode-escapes
  (is (equal (pomo::parse-comments "U&'d\\0061t\\+000061'")
             "U&'d\\0061t\\+000061'")))

(test dollar-quoted-string-constants1
  (is (equal (pomo::parse-comments "$$Dianne's horse$$")
             "$$Dianne's horse$$")))

(test dollar-quoted-string-constants2
  (is (equal (pomo::parse-comments "$function$
BEGIN
    RETURN ($1 ~ $q$[\\t\\r\\n\\v\\]$q$);
END;
$function$")
             "$function$
BEGIN
    RETURN ($1 ~ $q$[\\t\\r\\n\\v\\]$q$);
END;
$function$")))

(test single-quote-sql
  (is (equal (pomo::parse-comments "REAL '1.23'  -- string style")
             "REAL '1.23'  ")))

(test simple-execute-file
  (with-test-connection
    (when (table-exists-p 'company-employees)
      (query (:drop-table :if-exists 'company-employees :cascade)))
    (pomo:execute-file *good-file*)
    (is (table-exists-p 'company-employees))
    (is (equal "Paul" (query (:select 'name :from 'company-employees :where (:= 'id 1)) :single)))
    (is (equal 11 (query (:select (:count 'id) :from 'company-employees) :single)))))

(test broken-execute-file
  (with-test-connection
    (when (table-exists-p 'company-employees)
      (query (:drop-table :if-exists 'company-employees :cascade)))
    (signals error (pomo:execute-file *bad-file*))
    (is (table-exists-p 'company-employees))
    (is (equal "paul" (query (:select 'name :from 'company-employees :where (:= 'id 1)) :single)))
;; the bad-file should stop executing on the attempt to insert a record with the same id as the first insertion
    (is (equal 1 (query (:select (:count 'id) :from 'company-employees) :single)))
    (query (:drop-table :if-exists 'company-employees :cascade))))

(test broken-execute-file-wrapped-in-transaction
  (with-test-connection
    (when (table-exists-p 'company-employees)
      (query (:drop-table :if-exists 'company-employees :cascade)))
    (signals error (pomo:execute-file *bad-file-with-transaction*)))
  (with-test-connection
    (is (not (table-exists-p 'company-employees)))
    (query (:drop-table :if-exists 'company-employees :cascade))))

(test fail-include-execute-file
  (with-test-connection
    (when (table-exists-p 'company-employees)
      (query (:drop-table :if-exists 'company-employees :cascade)))
    (signals error (pomo:execute-file *fail-include-file*))
    (query (:drop-table :if-exists 'company-employees :cascade))))
