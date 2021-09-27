;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN; -*-
(in-package :postmodern)

;; Connection parameters

(defvar *max-pool-size* nil
  "Set the maximum amount of connections kept in a single connection pool, where
a pool consists of all the stored connections with the exact same connect
arguments. Defaults to NIL, which means there is no maximum.")

(defparameter *default-use-ssl* :no "The default for connect's use-ssl argument.
This starts at :no. If you set it to anything else, be sure to also load the
CL+SSL library.")

(defparameter *schema-path* nil "If the default path is reset, it will also reset
this parameter which will get read by reconnect.")

;; Transaction Parameters
(defparameter *transaction-level* 0)

(defparameter *isolation-level* :read-committed-rw "The transaction isolation
level currently in use. You can specify the following isolation levels in
postmodern transactions:

- :read-committed-rw (read committed with read and write)
- :read-committed-ro (read committed with read only)
- :repeatable-read-rw (repeatable read with read and write)
- :repeatable-read-ro (repeatable read with read only)
- :serializable (serializable with reand and write)")

;; Preparation Parameters
(defparameter *allow-overwriting-prepared-statements* t
  "When set to t, ensured-prepared will overwrite prepared statements having the
same name if the query statement itself in the postmodern meta connection is
different than the query statement provided to ensure-prepared.")

(defparameter *enforce-parameter-types* nil
  "When set to t, the parameters of the first invocation of a prepared statement
will set the mandatory types that subsequent invocations of that prepared
statement must meet. If the parameters are used in paramparameters must meet.  parameters must match type ensured-prepared will overwrite prepared statements having the
same name if the query statement itself in the postmodern meta connection is
different than the query statement provided to ensure-prepared.")

;; Query Parameters
(defparameter *result-styles*
  '((:none ignore-row-reader all-rows)
    (:debug cl-postgres::debug-row-reader all-rows)
    (:lists list-row-reader all-rows)
    (:list list-row-reader single-row)
    (:rows list-row-reader all-rows)
    (:row list-row-reader single-row)
    (:vectors vector-row-reader all-rows)
    (:alists symbol-alist-row-reader all-rows)
    (:alist symbol-alist-row-reader single-row)
    (:str-alists alist-row-reader all-rows)
    (:str-alist alist-row-reader single-row)
    (:plists symbol-plist-row-reader all-rows)
    (:plist symbol-plist-row-reader single-row)
    (:array-hash array-hash-row-reader all-rows)
    (:column column-row-reader all-rows)
    (:single column-row-reader single-row)
    (:single! column-row-reader single-row!)
    (:json-strs json-row-reader all-rows)
    (:json-str json-row-reader single-row)
    (:json-array-str json-row-array-reader all-rows))
  "Mapping from keywords identifying result styles to the row-reader
that should be used and whether all values or only one value should be
returned.")


;; Utilities Parameters
(defparameter *character-sets*
  '("EUC_CN" "EUC_JP" "EUC_JIS_2004" "EUC_KR" "EUC_TW" "ISO_8859_5" "ISO_8859_6"
    "ISO_8859_7" "ISO_8859_8" "KOI8R" "KOI8U" "LATIN1" "LATIN2" "LATIN3" "LATIN4"
    "LATIN5" "LATIN6" "LATIN7" "LATIN8" "LATIN9" "LATIN10" "MULE_INTERNAL"
    "SQL_ASCII" "UTF8" "WIN866" "WIN874" "WIN1250" "WIN1251" "WIN1252" "WIN1253"
    "WIN1254" "WIN1255" "WIN1256" "WIN1257" "WIN1258"))

(defparameter *collations* nil)

;; Dao Parameters (table.lisp)
(defparameter *custom-column-writers* nil
  "A hook for locally overriding/adding behaviour to DAO row readers.
Should be an alist mapping strings (column names) to symbols or
functions. Symbols are interpreted as slot names that values should be
written to, functions are called with the new object and the value as
arguments.")

(defparameter *ignore-unknown-columns* nil "Normally, when get-dao, select-dao,
save-dao or query-dao finds a column in the database that's not in the DAO class,
it should raise an error. Setting this variable to a non-NIL will cause it to
simply ignore the unknown column and allows you to define daos which are subsets
of a table.")

;; Roles Parameters
(defparameter *alter-all-default-select-privileges*
  '("grant usage on schema ~a to ~a"
    "grant select on all tables in schema ~a to ~a"
    "grant select on all sequences in schema ~a to ~a"
    "alter default privileges in schema ~a grant select on tables to ~a"))

(defparameter *alter-all-default-editor-privileges*
  '("grant usage on schema ~a to ~a"
    "grant select, insert, update, delete on all tables in schema ~a to ~a"
    "alter default privileges in schema ~a grant select, insert, update, delete on tables to ~a"    ))

(defparameter *execute-privileges-list*
  '("grant execute on all functions in schema ~a to ~a"))

(defparameter *disallowed-role-names*
  (let ((words (make-hash-table :test 'equal)))
    (dolist (word '(".htaccess" ".htpasswd" ".json" ".rss" ".well-known" ".xml"
                    "about" "abuse" "access" "account" "accounts" "activate"
                    "ad" "add" "address" "adm" "admin" "admins" "administration"
                    "administrator" "administrators" "ads" "adult" "advertising"
                    "affiliate" "affiliates" "ajax" "all" "analytics" "android"
                    "anon" "anonymous" "api" "app" "apps" "archive" "atom"
                    "auth" "authentication" "authorize" "autoconfig"
                    "autodiscover" "avatar" "backend" "backoffice" "backup"
                    "bad" "banner" "banners" "best" "beta" "billing" "bin"
                    "blackberry" "blog" "blogs" "board" "bot" "bots"
                    "broadcasthost" "business" "bug" "buy" "cache" "calendar"
                    "campaign" "career" "careers" "cart" "cdn" "ceo" "cfo"
                    "cgi" "chat" "chef" "client" "clientaccesspolicy.xml"
                    "clients" "code" "codes" "commercial" "compare" "config"
                    "configuration" "connect" "contact" "contact-us" "contactus"
                    "contest" "coo" "cookie" "copyright" "corporate" "create"
                    "crossdomain" "crossdomain.xml" "css" "cto" "customer"
                    "dash" "dashboard" "data" "database" "db" "delete" "demo"
                    "design" "designer" "dev" "devel" "developer" "developers"
                    "development" "dir" "directory" "dmca" "doc" "docs"
                    "document" "documents" "documentation" "domain""domainadmin"
                    "domainadministrator" "download" "downloads" "ecommerce"
                    "edit" "editor" "email" "embed" "enquiry" "enterprise"
                    "facebook" "faq" "favicon.ico" "favorite" "favorites"
                    "favourite" "favourites" "feed" "feedback" "feeds"
                    "file" "files" "follow" "font" "fonts" "forum" "forums"
                    "free" "ftp" "gadget" "gadgets" "games" "gift" "good"
                    "google" "group" "groups" "guest" "guests" "help"
                    "helpcenter" "home" "homepage" "host" "hosting" "hostmaster"
                    "host-master" "host_master" "hostname" "html" "http" "httpd"
                    "https" "humans.txt" "image" "images" "imap" "img"
                    "index" "indice" "info" "information" "inquiry" "intranet"
                    "invite" "ipad" "iphone" "irc" "is" "isatap" "it" "java"
                    "javascript" "job" "jobs" "js" "json" "keybase.txt"
                    "knowledgebase" "ldap" "legal" "license" "list"
                    "list-request" "lists" "localdomain" "localhost" "log"
                    "login" "logout" "logs" "mail" "mailer-daemon" "majordomo"
                    "manager" "manifesto" "map" "marketing" "master" "me"
                    "media" "member" "membership" "message" "messages"
                    "messenger" "mine" "mis" "mob" "mobile" "msg" "must" "mx"
                    "my" "myaccount" "mysql" "name" "named" "net" "network"
                    "new" "newest" "news" "newsletter" "no-reply" "nobody" "noc"
                    "noreply" "notes" "oauth" "oembed" "office" "old" "oldest"
                    "online" "operator" "order" "orders" "owner " "page" "pager"
                    "pages" "panel" "password" "pay" "payment" "payments" "perl"
                    "photo" "photos" "php" "pic" "pics" "plan" "plans" "plugin"
                    "plugins" "pop" "pop3" "portfolio" "post" "postfix"
                    "postmaster" "post-master" "post_master" "posts"
                    "preferences" "press" "pricing" "privacy" "privacy-policy"
                    "private" "profile" "project" "projects" "promo" "pub"
                    "public" "python" "random" "recipe" "recipes" "register"
                    "registration" "remove" "request" "reset" "robots"
                    "robots.txt" "root" "rss" "ruby" "sale" "sales" "sample"
                    "samples" "save" "script" "scripts" "search" "secure"
                    "security" "send" "service" "services" "setting" "settings"
                    "setup" "sftp" "shop" "shopping" "signin" "signout" "signup"
                    "site" "sitemap" "sitemap.xml" "sites" "smtp" "sql" "src"
                    "ssh" "ssl" "ssladmin" "ssladministrator" "sslwebmaster"
                    "ssl_admin" "ssl-admin" "ssl_administrator"
                    "ssl-administrator" "ssl_webmaster" "ssl-webmaster"
                    "stage" "staging" "start" "stat" "static" "stats" "status"
                    "store" "stores" "subdomain" "subscribe" "support"
                    "surprise" "svn" "sys" "sysadmin" "sysadministrator" "sysop"
                    "system" "tablet" "tablets" "talk" "task" "tasks" "tech"
                    "telnet" "terms" "terms-of-use" "test" "test1" "test2"
                    "test3" "tests" "theme" "themes" "tmp" "todo" "tools" "top"
                    "tos" "trouble" "trust" "tv" "twitter" "twittr"
                    "unsubscribe" "update" "upload" "url" "usage" "usenet" "user"
                    "username" "users" "uucp" "video" "videos" "visitor" "web"
                    "weblog" "webmail""web_master" "web-master" "web_admin"
                    "web-admin" "wheel" "webmaster" "website" "websites"
                    "welcome" "wiki" "win" "work" "wpad" "ww" "wws" "www" "www1"
                    "www2" "www3" "www4" "www5" "www6" "www7" "wwws" "wwww"
                    "xml" "xpg" "xxx" "yahoo" "you" "yourdomain" "yourname"
                    "yoursite" "yourusername"))
      (setf (gethash word words) t))
    words)
  "A set of words that maybe should be disallowed from user names. Edit as you
please.")

;; json-encoder Parameters
;;;; Borrowed from cl-json but modified to deal with timestamps, intervals
;;;; and other specific objects
;;;; Copyright (c) 2006-2008 Henrik Hjelte
;;;; Copyright (c) 2008 Hans Hübner (marked parts)
;;;; All rights reserved.
;;;; MIT License
;;; Symbols
(defparameter +json-lisp-symbol-tokens+
  '(("true" . t)
    ("null" . nil)
    ("false" . nil))
  "Mapping between JSON literal names and Lisp boolean values.")

(defvar *json-symbols-package* (find-package 'keyword)
  "The package where JSON Object keys etc. are interned.
Default KEYWORD, NIL = use current *PACKAGE*.")

(defparameter *json-list-encoder-fn* 'encode-json-list-guessing-encoder)

(defvar *json-identifier-name-to-lisp* 'camel-case-to-lisp
  "Designator for a function which maps string (a JSON Object key) to
string (name of a Lisp symbol).")

(defvar *lisp-identifier-name-to-json* 'lisp-to-camel-case
  "Designator for a function which maps string (name of a Lisp symbol)
to string (e. g. JSON Object key).")

(defvar *identifier-name-to-key* 'json-intern
  "Designator for a function which, during decoding, maps the *json-identifier-name-to-lisp*
-transformed key to the value it will have in the result object.")

(defvar *json-output* (make-synonym-stream '*standard-output*)
  "The default output stream for encoding operations.")

;;; The code below is from Hans Hübner's YASON (with modifications).

(defvar *json-aggregate-context* nil
  "NIL outside of any aggregate environment, 'ARRAY or 'OBJECT within
the respective environments.")

(defvar *json-aggregate-first* t
  "T when the first member of a JSON Object or Array is encoded,
afterwards NIL.")
