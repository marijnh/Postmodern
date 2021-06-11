;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN-TESTS; -*-
(in-package :postmodern-tests)

(def-suite :postmodern-binary
    :description "Dao suite for postmodern"
    :in :postmodern)

(in-suite :postmodern-binary)

(test binary-parameters
  (with-binary-test-connection
    (query "create table countries (id serial not null, iso2 text not null,
                                            latitude numeric(9, 6) not null,
                                            longitude numeric(9, 6) not null,
                                            name text not null,
                                            population integer not null)"
                'list-row-reader)
    (query "insert into countries (iso2, latitude, longitude, name, population)
                     values   ('AD', 	42.546245, 	1.601554, 	'Andorra', 77142),
                              ('AE', 	23.424076, 	53.847818, 	'UAE', 9770529),
                              ('AF', 	33.93911, 	67.709953, 	'Afghanistan', 38041754),
                              ('AG', 	17.060816, 	-61.796428, 	'Antigua and Barbuda', 97118),
                              ('AI', 	18.220554, 	-63.068615, 	'Anguilla', 13869),
                              ('AL', 	41.153332, 	20.168331, 	'Albania', 2880917),
                              ('AM', 	40.069099, 	45.038189, 	'Armenia', 2957731),
                              ('AW', 	12.52111, 	-69.968338, 	'Aruba', 106314),
                              ('AO', 	-11.202692, 	17.873887, 	'Angola', 31825295),
                              ('AR', 	-38.416097, 	-63.616672, 	'Argentina', 44780677),
                              ('AS', 	-14.270972, 	-170.132217, 	'American Samoa', 55312),
                              ('AT', 	47.516231, 	14.550072, 	'Austria', 8955102),
                              ('AU', 	-25.274398, 	133.775136, 	'Australia', 25203198)"
                'list-row-reader)
    (is (equal (query "select $1" 132 :single)
               132))
    (is (equal (query "select $1, $2" "10" 20)
               '(("10" 20))))
    (is (equal (query "select name from countries where population = $1" 97118 :single)
               "Antigua and Barbuda"))
    (is (equal (query
                "select name from countries where population < $1 and population > $2"
                3000000 100000)
               '(("Albania") ("Armenia") ("Aruba"))))
    (is (equal (query
                "select name from countries where latitude < $1 and latitude > $2"
                30 10)
               '(("UAE") ("Antigua and Barbuda") ("Anguilla") ("Aruba"))))
    (is (equal (query "select $1, $2" "10" (/ 1 3.0))
               '(("10" 0.33333334))))
    (is (equal (query "select $1, $2" "10" (/ 1 3.0d0))
               '(("10" 0.3333333333333333d0))))
    (is (equal (query "select $1, $2" "10" t)
               '(("10" T))))
    (is (equal (query "select $1, $2" "10" nil)
               '(("10" NIL))))
    (drop-table 'countries)))
