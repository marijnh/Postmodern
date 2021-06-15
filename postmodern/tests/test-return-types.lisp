;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN-TESTS; -*-
(in-package :postmodern-tests)

(def-suite :postmodern-return-types
  :description "Return types"
  :in :postmodern)

(in-suite :postmodern-return-types)

(defun return-types-fixture ()
  (when (table-exists-p 'test-data)
    (query (:drop-table :if-exists 'test-data :cascade)))
  (execute (:create-table test-data ((id :type integer :primary-key t)
                                     (int4 :type integer)
                                     (text :type (or text db-null))
                                     (jsonb :type (or jsonb db-null))
                                     (timestamp-without-time-zone
                                      :type (or timestamp-without-time-zone
                                                db-null))
                                     (timestamp-with-time-zone
                                      :type (or timestamp-with-time-zone db-null))
                                     (timestamptz :type (or timestamptz db-null))
                                     (timestamp :type (or timestamp db-null))
                                     (time :type (or time db-null))
                                     (date :type (or date db-null))
                                     (interval :type (or interval db-null)))))

  (query (:insert-rows-into 'test-data
          :columns 'id 'int4 'text 'jsonb 'timestamp-without-time-zone
          'timestamp-with-time-zone 'timestamptz 'timestamp 'time 'date 'interval
          :values '((1 2147483645 "text one" "{\"date\": \"-300\", \"lang\": \"en\", \"category1\": \"By place\", \"category2\": \"Greece\", \"description\": \"Pilgrims travel to the healing temples of Asclepieion to be cured of their ills. After a ritual purification the followers bring offerings or sacrifices.\", \"granularity\": \"year\"}"
                     "2020-12-30 13:30:54" "2019-12-30 13:30:54" "2018-12-30 13:30:54"
                     "2017-12-30 13:30:54"
                     "14:31:54" "2016-12-30" "2 hours 10 minutes")
                    (2 0 "text two" "{\"date\": \"-300\", \"lang\": \"en\", \"category1\": \"By place\", \"category2\": \"Egypt\", \"description\": \"Pyrrhus, the King of Epirus, is taken as a hostage to Egypt after the Battle of Ipsus and makes a diplomatic marriage with the princess Antigone, daughter of Ptolemy and Berenice.\", \"granularity\": \"year\"}"
                     "1920-12-30 13:30:54" "1919-12-30 13:30:54" "1918-12-30 13:30:54"
                     "1917-12-30 13:30:54"
                     "14:32:54" "1916-12-30" "3 months 2 hours 10 minutes")
                    (3 3 "text three" "{\"date\": \"-300\", \"lang\": \"en\", \"category1\": \"By place\", \"category2\": \"Egypt\", \"description\": \"Ptolemy concludes an alliance with King Lysimachus of Thrace and gives him his daughter Arsinoe II in marriage.\", \"granularity\": \"year\"}"
                     "1980-12-30 13:30:54" "1981-12-30 13:30:54" "1982-12-30 13:30:54"
                     "1983-12-30 13:30:54"
                     "14:33:54" "1983-12-30" "5 years 3 months 2 hours 10 minutes")))))

(defmacro with-return-types-fixture (&body body)
  `(progn
     (return-types-fixture)
     (unwind-protect (progn ,@body)
       (execute (:drop-table :if-exists 'test-data :cascade)))))

(test return-types-json
  (with-test-connection
    (with-return-types-fixture
      (is (equal (query (:select 'id 'jsonb :from 'test-data
                                            :where (:< 'id 3)))
                 '((1
                    "{\"date\": \"-300\", \"lang\": \"en\", \"category1\": \"By place\", \"category2\": \"Greece\", \"description\": \"Pilgrims travel to the healing temples of Asclepieion to be cured of their ills. After a ritual purification the followers bring offerings or sacrifices.\", \"granularity\": \"year\"}")
                   (2
                    "{\"date\": \"-300\", \"lang\": \"en\", \"category1\": \"By place\", \"category2\": \"Egypt\", \"description\": \"Pyrrhus, the King of Epirus, is taken as a hostage to Egypt after the Battle of Ipsus and makes a diplomatic marriage with the princess Antigone, daughter of Ptolemy and Berenice.\", \"granularity\": \"year\"}"))))
      (is (equal (query (:select 'jsonb :from 'test-data :where (:= 'id 3)) :single)
                 "{\"date\": \"-300\", \"lang\": \"en\", \"category1\": \"By place\", \"category2\": \"Egypt\", \"description\": \"Ptolemy concludes an alliance with King Lysimachus of Thrace and gives him his daughter Arsinoe II in marriage.\", \"granularity\": \"year\"}"))
      (is (equal (query (:select 'id 'jsonb :from 'test-data
                                            :where (:= 'id 3)) :list)
                 '(3
                   "{\"date\": \"-300\", \"lang\": \"en\", \"category1\": \"By place\", \"category2\": \"Egypt\", \"description\": \"Ptolemy concludes an alliance with King Lysimachus of Thrace and gives him his daughter Arsinoe II in marriage.\", \"granularity\": \"year\"}")))
      (is (equal (query (:select 'id 'jsonb :from 'test-data
                                            :where (:< 'id 3)) :lists)
                 '((1
                    "{\"date\": \"-300\", \"lang\": \"en\", \"category1\": \"By place\", \"category2\": \"Greece\", \"description\": \"Pilgrims travel to the healing temples of Asclepieion to be cured of their ills. After a ritual purification the followers bring offerings or sacrifices.\", \"granularity\": \"year\"}")
                   (2
                    "{\"date\": \"-300\", \"lang\": \"en\", \"category1\": \"By place\", \"category2\": \"Egypt\", \"description\": \"Pyrrhus, the King of Epirus, is taken as a hostage to Egypt after the Battle of Ipsus and makes a diplomatic marriage with the princess Antigone, daughter of Ptolemy and Berenice.\", \"granularity\": \"year\"}"))))
      (is (equal (query (:select 'id 'jsonb :from 'test-data
                                            :where (:= 'id 3)) :alist)
                 '((:ID . 3)
                   (:JSONB
                    . "{\"date\": \"-300\", \"lang\": \"en\", \"category1\": \"By place\", \"category2\": \"Egypt\", \"description\": \"Ptolemy concludes an alliance with King Lysimachus of Thrace and gives him his daughter Arsinoe II in marriage.\", \"granularity\": \"year\"}"))))
      (is (equal (query (:select 'id 'jsonb :from 'test-data
                                            :where (:= 'id 3)) :str-alist)
                 '(("id" . 3)
                   ("jsonb"
                    . "{\"date\": \"-300\", \"lang\": \"en\", \"category1\": \"By place\", \"category2\": \"Egypt\", \"description\": \"Ptolemy concludes an alliance with King Lysimachus of Thrace and gives him his daughter Arsinoe II in marriage.\", \"granularity\": \"year\"}"))))
      (is (equal (query (:select 'id 'jsonb :from 'test-data
                                            :where (:< 'id 3)) :alists)
                 '(((:ID . 1)
                    (:JSONB
                     . "{\"date\": \"-300\", \"lang\": \"en\", \"category1\": \"By place\", \"category2\": \"Greece\", \"description\": \"Pilgrims travel to the healing temples of Asclepieion to be cured of their ills. After a ritual purification the followers bring offerings or sacrifices.\", \"granularity\": \"year\"}"))
                   ((:ID . 2)
                    (:JSONB
                     . "{\"date\": \"-300\", \"lang\": \"en\", \"category1\": \"By place\", \"category2\": \"Egypt\", \"description\": \"Pyrrhus, the King of Epirus, is taken as a hostage to Egypt after the Battle of Ipsus and makes a diplomatic marriage with the princess Antigone, daughter of Ptolemy and Berenice.\", \"granularity\": \"year\"}")))))
      (is (equal (query (:select 'id 'jsonb :from 'test-data
                                            :where (:< 'id 3)) :str-alists)
                 '((("id" . 1)
                    ("jsonb"
                     . "{\"date\": \"-300\", \"lang\": \"en\", \"category1\": \"By place\", \"category2\": \"Greece\", \"description\": \"Pilgrims travel to the healing temples of Asclepieion to be cured of their ills. After a ritual purification the followers bring offerings or sacrifices.\", \"granularity\": \"year\"}"))
                   (("id" . 2)
                    ("jsonb"
                     . "{\"date\": \"-300\", \"lang\": \"en\", \"category1\": \"By place\", \"category2\": \"Egypt\", \"description\": \"Pyrrhus, the King of Epirus, is taken as a hostage to Egypt after the Battle of Ipsus and makes a diplomatic marriage with the princess Antigone, daughter of Ptolemy and Berenice.\", \"granularity\": \"year\"}")))))
      (is (equal (query (:select 'id 'jsonb :from 'test-data
                                            :where (:= 'id 3)) :plist)
                 '(:ID 3 :JSONB
                   "{\"date\": \"-300\", \"lang\": \"en\", \"category1\": \"By place\", \"category2\": \"Egypt\", \"description\": \"Ptolemy concludes an alliance with King Lysimachus of Thrace and gives him his daughter Arsinoe II in marriage.\", \"granularity\": \"year\"}")))
      (is (equal (query (:select 'id 'jsonb :from 'test-data
                                            :where (:< 'id 3)) :plists)
                 '((:ID 1 :JSONB
                    "{\"date\": \"-300\", \"lang\": \"en\", \"category1\": \"By place\", \"category2\": \"Greece\", \"description\": \"Pilgrims travel to the healing temples of Asclepieion to be cured of their ills. After a ritual purification the followers bring offerings or sacrifices.\", \"granularity\": \"year\"}")
                   (:ID 2 :JSONB
                    "{\"date\": \"-300\", \"lang\": \"en\", \"category1\": \"By place\", \"category2\": \"Egypt\", \"description\": \"Pyrrhus, the King of Epirus, is taken as a hostage to Egypt after the Battle of Ipsus and makes a diplomatic marriage with the princess Antigone, daughter of Ptolemy and Berenice.\", \"granularity\": \"year\"}"))))
      (is (typep (query (:select 'id 'jsonb :from 'test-data
                                            :where (:< 'id 3)) :array-hash) 'vector))
      (is (typep (aref (query (:select 'id 'jsonb :from 'test-data
                                                  :where (:< 'id 3)) :array-hash) 1) 'hash-table))
      (let ((val (alexandria:hash-table-alist
                  (aref
                   (query (:select 'id 'jsonb :from 'test-data
                                              :where (:< 'id 3)) :array-hash)
                   1))))
        (is (or
             (equal val
                    '(("jsonb"
                       . "{\"date\": \"-300\", \"lang\": \"en\", \"category1\": \"By place\", \"category2\": \"Egypt\", \"description\": \"Pyrrhus, the King of Epirus, is taken as a hostage to Egypt after the Battle of Ipsus and makes a diplomatic marriage with the princess Antigone, daughter of Ptolemy and Berenice.\", \"granularity\": \"year\"}")
                      ("id" . 2)))
             (equal val
                    '(("id" . 2)
                      ("jsonb"
                       . "{\"date\": \"-300\", \"lang\": \"en\", \"category1\": \"By place\", \"category2\": \"Egypt\", \"description\": \"Pyrrhus, the King of Epirus, is taken as a hostage to Egypt after the Battle of Ipsus and makes a diplomatic marriage with the princess Antigone, daughter of Ptolemy and Berenice.\", \"granularity\": \"year\"}"))))))
      (is (equal (query (:select 'id 'jsonb :from 'test-data
                                            :where (:< 'id 3)) :json-strs)
                 '("{\"id\":1,\"jsonb\":\"{\\\"date\\\": \\\"-300\\\", \\\"lang\\\": \\\"en\\\", \\\"category1\\\": \\\"By place\\\", \\\"category2\\\": \\\"Greece\\\", \\\"description\\\": \\\"Pilgrims travel to the healing temples of Asclepieion to be cured of their ills. After a ritual purification the followers bring offerings or sacrifices.\\\", \\\"granularity\\\": \\\"year\\\"}\"}"
                   "{\"id\":2,\"jsonb\":\"{\\\"date\\\": \\\"-300\\\", \\\"lang\\\": \\\"en\\\", \\\"category1\\\": \\\"By place\\\", \\\"category2\\\": \\\"Egypt\\\", \\\"description\\\": \\\"Pyrrhus, the King of Epirus, is taken as a hostage to Egypt after the Battle of Ipsus and makes a diplomatic marriage with the princess Antigone, daughter of Ptolemy and Berenice.\\\", \\\"granularity\\\": \\\"year\\\"}\"}")))
      (is (equal (query (:select 'id 'jsonb :from 'test-data
                                            :where (:= 'id 3)) :json-str)
                 "{\"id\":3,\"jsonb\":\"{\\\"date\\\": \\\"-300\\\", \\\"lang\\\": \\\"en\\\", \\\"category1\\\": \\\"By place\\\", \\\"category2\\\": \\\"Egypt\\\", \\\"description\\\": \\\"Ptolemy concludes an alliance with King Lysimachus of Thrace and gives him his daughter Arsinoe II in marriage.\\\", \\\"granularity\\\": \\\"year\\\"}\"}"))
      (is (equal (query (:select 'id 'jsonb :from 'test-data
                                            :where (:< 'id 3)) :json-array-str)
                 "[{\"id\":1,\"jsonb\":\"{\\\"date\\\": \\\"-300\\\", \\\"lang\\\": \\\"en\\\", \\\"category1\\\": \\\"By place\\\", \\\"category2\\\": \\\"Greece\\\", \\\"description\\\": \\\"Pilgrims travel to the healing temples of Asclepieion to be cured of their ills. After a ritual purification the followers bring offerings or sacrifices.\\\", \\\"granularity\\\": \\\"year\\\"}\"}, {\"id\":2,\"jsonb\":\"{\\\"date\\\": \\\"-300\\\", \\\"lang\\\": \\\"en\\\", \\\"category1\\\": \\\"By place\\\", \\\"category2\\\": \\\"Egypt\\\", \\\"description\\\": \\\"Pyrrhus, the King of Epirus, is taken as a hostage to Egypt after the Battle of Ipsus and makes a diplomatic marriage with the princess Antigone, daughter of Ptolemy and Berenice.\\\", \\\"granularity\\\": \\\"year\\\"}\"}]")))))

(test return-types
  (with-test-connection
    (with-return-types-fixture
      (is (equal (query (:select 'id 'int4 'text :from 'test-data
                         :where (:= 'id 3)) :none)
                 nil))
      (is (equal (query (:select 'id 'int4 'text :from 'test-data
                         :where (:< 'id 3)))
                 '((1 2147483645 "text one") (2 0 "text two"))))
      (is (equal (query (:select 'text :from 'test-data :where (:= 'id 3)) :single)
                 "text three"))
      (is (equal (query (:select 'id 'int4 'text :from 'test-data
                         :where (:= 'id 3)) :list)
                 '(3 3 "text three")))
      (is (equal (query (:select 'id 'int4 'text :from 'test-data
                         :where (:< 'id 3)) :lists)
                 '((1 2147483645 "text one") (2 0 "text two"))))
      (is (equal (query (:select 'id 'int4 'text :from 'test-data
                         :where (:= 'id 3)) :alist)
                 '((:ID . 3) (:INT4 . 3) (:TEXT . "text three"))))
      (is (equal (query (:select 'id 'int4 'text :from 'test-data
                         :where (:= 'id 3)) :str-alist)
                 '(("id" . 3) ("int4" . 3) ("text" . "text three"))))
      (is (equal (query (:select 'id 'int4 'text :from 'test-data
                         :where (:< 'id 3)) :alists)
                 '(((:ID . 1) (:INT4 . 2147483645) (:TEXT . "text one"))
                   ((:ID . 2) (:INT4 . 0) (:TEXT . "text two")))))
      (is (equal (query (:select 'id 'int4 'text :from 'test-data
                         :where (:< 'id 3)) :str-alists)
                 '((("id" . 1) ("int4" . 2147483645) ("text" . "text one"))
                   (("id" . 2) ("int4" . 0) ("text" . "text two")))))
      (is (equal (query (:select 'id 'int4 'text :from 'test-data
                         :where (:= 'id 3)) :plist)
                 '(:ID 3 :INT4 3 :TEXT "text three")))
      (is (equal (query (:select 'id 'int4 'text :from 'test-data
                         :where (:< 'id 3)) :plists)
                 '((:ID 1 :INT4 2147483645 :TEXT "text one")
                   (:ID 2 :INT4 0 :TEXT "text two"))))

      (is (equalp (query (:select 'id 'int4 'text :from 'test-data :where (:< 'id 3)) :vectors)
                  #(#(1 2147483645 "text one") #(2 0 "text two"))))
      (is (equalp (query (:select 'id :from 'test-data) :vectors)
                  #(#(1) #(2) #(3))))
      (is (equalp (query (:select 'id 'int4 'text :from 'test-data) :vectors)
                  #(#(1 2147483645 "text one") #(2 0 "text two") #(3 3 "text three"))))
      (is (equalp (aref (query (:select 'id 'int4 'text :from 'test-data) :vectors) 1)
                  #(2 0 "text two")))

      (let ((val (alexandria:hash-table-alist
                  (aref
                   (query (:select 'id 'int4 'text :from 'test-data
                           :where (:< 'id 3)) :array-hash)
                   1))))
        (is (equal (assoc "text" val :test 'equal)
                   '("text" . "text two"))))
      (is (equal (query (:select 'id :from 'test-data
                         :where (:< 'id 3)) :column)
                 '(1 2)))
      (is (equal (query (:select 'id :from 'test-data
                         :where (:= 'id 3)) :column)
                 '(3)))
      (is (equal (query (:select 'id 'int4 'text :from 'test-data
                         :where (:< 'id 3)) :json-strs)
                 '("{\"id\":1,\"int4\":2147483645,\"text\":\"text one\"}"
                   "{\"id\":2,\"int4\":0,\"text\":\"text two\"}")))
      (is (equal (query (:select 'id 'int4 'text :from 'test-data
                         :where (:= 'id 3)) :json-str)
                 "{\"id\":3,\"int4\":3,\"text\":\"text three\"}"))
      (is (equal (query (:select 'id 'int4 'text :from 'test-data
                         :where (:< 'id 3)) :json-array-str)
                 "[{\"id\":1,\"int4\":2147483645,\"text\":\"text one\"}, {\"id\":2,\"int4\":0,\"text\":\"text two\"}]")))))

(test empty-return-types
  (with-test-connection
    (with-return-types-fixture
      (is (equalp (query (:select 'id 'int4 'text :from 'test-data :where (:< 'id 1))
                         :vectors)
                  #()))
      (is (equalp (query (:select 'id 'int4 'text :from 'test-data :where (:< 'id 1))
                         :array-hash)
                  #()))
      (is (equalp (query (:select 'id 'int4 'text :from 'test-data :where (:< 'id 1))
                         :json-array-str)
                  "[]"))
      (is-false (query (:select 'id 'int4 'text :from 'test-data :where (:< 'id 1))
                       :json-strs))
      (is-false (query (:select 'id 'int4 'text :from 'test-data :where (:< 'id 1))
                       :alists))
      (is-false (query (:select 'id 'int4 'text :from 'test-data :where (:< 'id 1))
                       :alist))
      (is-false (query (:select 'id 'int4 'text :from 'test-data :where (:< 'id 1))
                       :plists))
      (is-false (query (:select 'id 'int4 'text :from 'test-data :where (:< 'id 1))
                       :plist))
      (is-false (query (:select 'id 'int4 'text :from 'test-data :where (:< 'id 1)))))))

(test return-type-types
  (with-test-connection
    (with-return-types-fixture
      (is (typep (query (:select 'id 'int4 'text :from 'test-data) :vectors)
                 'vector))
      (is (typep (aref (query (:select 'id 'int4 'text :from 'test-data) :vectors)
                       1)
                 'vector))
      (is (typep (query (:select 'id 'int4 'text :from 'test-data :where (:< 'id 1)) :vectors)
                 'vector))
      (is (typep (query (:select 'id 'int4 'text :from 'test-data
                         :where (:< 'id 3)) :array-hash)
                 'vector))
      (is (typep (aref (query (:select 'id 'int4 'text :from 'test-data
                               :where (:< 'id 3)) :array-hash) 1)
                 'hash-table))
      (is (typep (query (:select 'id 'int4 'text :from 'test-data :where (:< 'id 1)) :array-hash)
                 'array)))))
