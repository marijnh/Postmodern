;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: S-SQL-TESTS; -*-
(in-package :s-sql-tests)

(def-suite :s-sql-create-index
    :description "Create Index suite for s-sql"
    :in :s-sql)

(in-suite :s-sql-create-index)


;;; TESTS ON INDEX FIELDS
(test create-index-1
  "Testing create-index. Available parameters - in order after name -
are :concurrently, :on, :using, :fields and :where.The advantage to using the
keyword :concurrently is that writes to the table
from other sessions are not locked out while the index is is built. The disadvantage is
that the table will need to be scanned twice. Everything is a trade-off."
  (is (equal (sql (:create-index 'films_idx :on 'films :fields 'title))
             "CREATE INDEX films_idx ON films (title)"))

  (is (equal (sql (:create-index 'films_idx :on "films" :using gin :fields 'title))
             "CREATE INDEX films_idx ON films USING gin (title)"))
  (is (equal (sql (:create-index 'doc-tags-id-tags
                   :on "doc-tags-array" :using gin :fields 'tags))
             "CREATE INDEX doc_tags_id_tags ON doc_tags_array USING gin (tags)"))
  (is (equal (sql (:create-index 'title-index :unique :on 'films :fields 'title
                   :with (:= 'fillfactor 70)))
             "CREATE UNIQUE INDEX title_index ON films (title) WITH (fillfactor = 70)"))
  (is (equal (sql (:create-unique-index 'doc-tags-id-doc-id
                   :on "doc-tags-array"  :fields 'doc-id))
             "CREATE UNIQUE INDEX doc_tags_id_doc_id ON doc_tags_array (doc_id)"))
  (is (equal (sql (:create-index 'films-idx :concurrently
                                 :on "films" :using 'btree :fields 'created-at))
             "CREATE INDEX CONCURRENTLY films_idx ON films USING btree (created_at)"))
  (is (equal (sql (:create-index 'films-idx :unique :concurrently :on "films"
                   :using 'btree :fields 'created-at))
             "CREATE UNIQUE INDEX CONCURRENTLY films_idx ON films USING btree (created_at)"))
  (is (equal (sql (:create-index (:if-not-exists 'test-uniq-1-idx)
                   :on test-uniq :fields 'name))
             "CREATE INDEX IF NOT EXISTS test_uniq_1_idx ON test_uniq (name)"))

;;; where clause tests for partial indexes
  (is (equal (sql (:create-index 'orders_unbilled_idx :on 'orders :fields order-nr
                   :where (:and (:is-not-true 'billed)
                                (:< 'order_nr 1000))))
             "CREATE INDEX orders_unbilled_idx ON orders (order_nr) WHERE ((billed IS NOT TRUE) and (order_nr < 1000))"))
  (with-test-connection
    (query (:drop-table :if-exists 'george :cascade))
    (is (eq (table-exists-p 'george) nil))
    (query (:create-table 'george ((id :type integer))))
    (is (eq (table-exists-p 'george) t))
    (query (:create-index 'george-idx :on 'george :fields 'id))
    (is (pomo:index-exists-p 'george-idx))
    (is (pomo:index-exists-p "george-idx"))
    (query (:drop-table :if-exists 'george :cascade))
    (is (not (pomo:index-exists-p 'george-idx)))

    (query (:drop-table :if-exists 'access-log :cascade))
    (query (:create-table 'access-log ((url :type varchar) (client-ip :type inet))))
    (query (:create-index 'access-log-client-ip-idx :on 'access-log :fields 'client-ip
                          :where (:not (:and (:> 'client-ip "192.168.100.0")
                                             (:< 'client-ip "192.168.100.255")))))
    (is (pomo:index-exists-p 'access-log-client-ip-idx))
    (query (:drop-table :if-exists 'access-log :cascade))))

(test create-index-fields-1
  (is (equal
       (sql (:create-index 'films-idx :on "films" :fields 'title))
       "CREATE INDEX films_idx ON films (title)"))
  (is (equal
       (sql (:create-index 'films-idx :on "films" :fields 'title 'id))
       "CREATE INDEX films_idx ON films (title, id)"))
  ;; Now without quoting the fields
  (is (equal
       (sql (:create-index 'films-idx :on "films" :fields title id))
       "CREATE INDEX films_idx ON films (title, id)")))

(test create-index-fields-expressions
  (is (equalp (sql (:create-index 'films_idx :on "films" :fields (:lower 'title)))
              "CREATE INDEX films_idx ON films (lower(title))"))
  (is (equalp (sql (:create-index 'films-idx :on 'films
                   :fields (:lower 'title) (:upper 'product-name)))
              "CREATE INDEX films_idx ON films (lower(title), upper(product_name))"))
  (is (equalp
       (sql (:create-index 'films-idx :on 'films
             :fields (:asc (:lower 'title)) (:nulls-last (:upper 'product-name))))
       "CREATE INDEX films_idx ON films (lower(title) ASC, upper(product_name) NULLS LAST)")))

(test create-index-fields-base-opclass
  (is (equal
       (sql (:create-index 'films-idx :on 'films :using gin
             :fields (:nulls-first 'customer-id) (:asc 'order-date)))
       "CREATE INDEX films_idx ON films USING gin (customer_id NULLS FIRST, order_date ASC)"))
  (is (equal
       (sql (:create-index 'films-idx :on 'films :using gin
             :fields (:nulls-last 'customer-id) (:asc 'order-date)))
       "CREATE INDEX films_idx ON films USING gin (customer_id NULLS LAST, order_date ASC)"))
  (is (equal
       (sql (:create-index 'films-idx :on 'films :using gin
             :fields (:nulls-first 'customer-id) 'order-date))
       "CREATE INDEX films_idx ON films USING gin (customer_id NULLS FIRST, order_date)")))

(test create-index-fields-btree-opclass
;;; CREATE INDEX ON test USING btree ((data ->> 'field'));
  (is (equal
             (sql (:create-index 'test-idx :on 'test :using btree
                   :fields (:->> 'data "field")))
             "CREATE INDEX test_idx ON test USING btree ((data ->> E'field'))"))
  (is (equal
             (sql (:create-index 'users-idx :on 'users
                   :fields (:text-pattern-ops 'email)))
             "CREATE INDEX users_idx ON users (email text_pattern_ops)")))

(test create-index-fields-gin-opclass
  ;; Create index statements pulled from https://pganalyze.com/blog/gin-index
  ;; CREATE INDEX ON test_items USING gin(metadata jsonb_path_ops);
  (is (equal
             (sql (:create-index 'items-idx :on 'test-items :using 'gin
                   :fields (:jsonb-path-ops 'metadata)))
             "CREATE INDEX items_idx ON test_items USING gin (metadata jsonb_path_ops)"))
;;; CREATE INDEX trgm_idx ON test_trgm USING gin (t1 gin_trgm_ops);
  (is (equal
             (sql (:create-index 'trgm-idx :on 'test-trgm :using gin
                   :fields (:gin-trgm-ops 'tc1)))
             "CREATE INDEX trgm_idx ON test_trgm USING gin (tc1 gin_trgm_ops)"))
;;; CREATE INDEX pgweb_idx ON pgweb USING GIN (to_tsvector('english', body));
  (is (equal
       (sql (:create-index 'pgweb-idx :on 'pgweb :using gin
             :fields (:to-tsvector "english" 'body)))
       "CREATE INDEX pgweb_idx ON pgweb USING gin (to_tsvector (E'english', body))")))

(test create-index-fields-box-operators
  (is (equal
             (sql (:create-index 'pointloc1 :on 'points :using gist
                   :fields (:asc (:box 'location))))
             "CREATE INDEX pointloc1 ON points USING gist (box(location) ASC)"))
  (is (equal
             (sql (:create-index 'pointloc0 :on 'points :using gist
                   :fields (:box 'location 'location)))
             "CREATE INDEX pointloc0 ON points USING gist (box(location, location))"))

  (is (equal (sql (:create-index 'pointloc1 :on 'points :using gist
                   :fields (:asc (:box 'location)) 'name))
             "CREATE INDEX pointloc1 ON points USING gist (box(location) ASC, name)"))
  ;; https://www.postgresql.org/docs/current/sql-createindex.html
  (is (equal
       (sql (:create-index 'pointloc2 :on 'points :using gist
                   :fields (:asc (:box 'location 'location)) (:nulls-last 'name)))
       "CREATE INDEX pointloc2 ON points USING gist (box(location, location) ASC, name NULLS LAST)"))
  (is (equal
             (sql (:create-index 'pointloc4 :on 'points :using gist
                   :fields (:asc (:box 'location 'location)) (:nulls-last 'name)))
             "CREATE INDEX pointloc4 ON points USING gist (box(location, location) ASC, name NULLS LAST)"))
  (is (equal
             (sql (:create-index 'pointloc3 :on 'points :using gist
                   :fields 'name (:asc (:box 'location))))
             "CREATE INDEX pointloc3 ON points USING gist (name, box(location) ASC)")))

(test create-index-storage
  ;; An additional difference between the first and second is field being quoted and not quoted
  (is (equal
             (sql (:create-index 'title-index :on 'films :fields 'title
                   :with (:= 'deduplicate-items 'off)))
             "CREATE INDEX title_index ON films (title) WITH (deduplicate_items = \"off\")"))
  (is (equal
             (sql (:create-index 'title-index :on 'films :fields 'title
                   :with (:= 'fillfactor 70)))
             "CREATE INDEX title_index ON films (title) WITH (fillfactor = 70)"))
  (is (equal
             (sql (:create-index 'gin-idx :on 'documents-table :using gin
                   :fields 'locations :with (:= 'fastupdate 'off)))
             "CREATE INDEX gin_idx ON documents_table USING gin (locations) WITH (fastupdate = \"off\")"))
  (is (equal (sql (:create-index 'gin-idx :on 'documents-table :using 'gin
                   :fields 'locations :with (:= 'fastupdate 'off) (:= 'fillfactor 70)))
             "CREATE INDEX gin_idx ON documents_table USING gin (locations) WITH (fastupdate = \"off\"), (fillfactor = 70)"))
  (is (equal (sql (:create-index 'gin-idx :on 'documents-table :using 'gin
                   :fields 'locations :with (:= 'deduplicate-items 'off)))
             "CREATE INDEX gin_idx ON documents_table USING gin (locations) WITH (deduplicate_items = \"off\")")))

(test to-tsvector
  (with-test-connection
    (query (:drop-table :if-exists 't11 :cascade))
    (query (:create-table 't11
                          ((title :type (or text db-null))
                           (body :type (or text db-null))
                           (tsv :type (or tsvector db-null)
                                :generated-always
                                (:to-tsvector "english" 'body)))))
    (query (:create-index 'textsearch11-idx :on 't11 :using 'gin :fields 'tsv))
    (is (pomo:index-exists-p 'textsearch11-idx))
    (query (:drop-table 't11 :cascade))
    (query (:drop-table :if-exists 't12 :cascade))
    (query (:create-table 't12
                          ((id :type bigint :primary-key :identity-by-default)
                           (body :type (or text db-null)))))
    (query (:alter-table 't12 :add-column 'tsv :type (or tsvector db-null)
            :generated-always
            (:to-tsvector "english" 'body)))
    (query (:create-index 'textsearch12-idx :on 't12 :using 'gin :fields 'tsv))
    (let ((quotes '(("Successful is the person who has lived well, laughed often and loved much, who has gained the respect of children, who leaves the world better than they found it, who has never lacked appreciation for the earth's beauty, who never fails to look for the best in others or give the best of themselves.")
                    ("Why should the way I feel depend on the thoughts in someone else's head?")
                    ("Finish each day and be done with it. You have done what you could. Some blunders and absurdities no doubt crept in; forget them as soon as you can. Tomorrow is a new day; begin it well and serenely and with too high a spirit to be encumbered with your old nonsense.")
                    ("Your actions speak so loud, I can't hear what you say.")
                    ("Hidden away in the inner nature of the real man is the law of his life, and someday he will discover it and consciously make use of it. He will heal himself, make himself happy and prosperous, and life in an entirely different world. For he will have discovered that life is from within and not from without.")
                    ("People only see what they are prepared to see. If you look for what is good and what you can be grateful for you will find it everywhere.")
                    ("Let me never fall into the vulgar mistake of dreaming that I am persecuted whenever I am contradicted.")
                    ("People do not seem to realize that their opinion of the world is also a confession of character."))))
      (query (:insert-rows-into 't12 :columns 'body :values quotes))
      (is (equal (query (:select 'id 'body :from 't12
                                           :where (:@@ (:to-tsvector "english" 'body)
                                                       (:to-tsquery "english" "spirit"))))
                 '((3
                    "Finish each day and be done with it. You have done what you could. Some blunders and absurdities no doubt crept in; forget them as soon as you can. Tomorrow is a new day; begin it well and serenely and with too high a spirit to be encumbered with your old nonsense."))))
      (is (equal (query (:select 'id 'body :from 't12
                                           :where (:@@ (:to-tsvector 'body)
                                                       (:to-tsquery "spirit"))))
                 '((3
                    "Finish each day and be done with it. You have done what you could. Some blunders and absurdities no doubt crept in; forget them as soon as you can. Tomorrow is a new day; begin it well and serenely and with too high a spirit to be encumbered with your old nonsense.")))))
    (is (equal (query (:select 'id 'body :from 't12 :where (:@@ (:to-tsvector 'body) (:to-tsquery "depend | loud"))))
               '((2 "Why should the way I feel depend on the thoughts in someone else's head?")
                 (4 "Your actions speak so loud, I can't hear what you say."))))
    (query (:drop-table :if-exists 't12 :cascade))))
