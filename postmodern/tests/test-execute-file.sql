drop table if exists company_employees;

create table company_employees(
   id bigserial primary key     not null,
   name           text    not null,
   age            int     not null,
   address        char(50),
   include_file         text,
   join_date	  date
);
-- ;Test comment 1;;
insert into company_employees (id,name,age,address,include_file,join_date) values (1, 'Paul', 32, 'London', 'test-execute-file','2001-07-13');
insert into company_employees (id,name,age,address,include_file,join_date) values (2, 'Ziad', 32, 'Beirut', 'test-execute-file','2003-03-13');
/* test' comment 2
with multiple lines
  --*/
/*
/*

*/
***/
  --\i ./postmodern/tests/tef-11.sql
\i tef-1.sql  -- an included file, will need to use fallback to find it
\ir     tef-6.sql -- an included file using file location relative to this file
  \ir   tef-3.sql
\i sub1/tef-4.sql
\ir ./sub1/tef-5.sql

  insert into company_employees (id,name,age,address,include_file,join_date) values (3, 'John', 32, 'Toronto', 'test-execute-file','2005-07-13');
  -- Yet another comments
insert into company_employees (id,name,age,address,include_file,join_date) values (4, 'Yasmin', 32, 'Mumbai', 'test-execute-file','2007-03-13');
/* ;test comment 3 (asterisk in /second/ line of multiline comment)
 * with multiple lines;;
*/
  insert into company_employees (id,name,age,address,include_file,join_date) values (5, 'Susan', 32, 'Vancouver', 'test-execute-file','2009-07-13');
/* ;test comment 4 (asterisk in second line of multiline comment)
 *** with multiple lines;;
  /* test' comment 4-1
   with multiple lines
  /***/
  */
 * did I say something wrong?
--*/
insert into company_employees (id,name,age,address,include_file,join_date) values (6, 'Johanna', 32, 'Berlin', 'test-execute-file','2011-03-13');
