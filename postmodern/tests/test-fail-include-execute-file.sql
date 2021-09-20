drop table if exists company_employees;

create table company_employees(
   id bigserial primary key     not null,
   name           text    not null,
   age            int     not null,
   address        char(50),
   include_file   text  ,
   join_date	  date
);

insert into company_employees (id,name,age,address,include_file,join_date) values (1, 'Paul', 32, 'London', 'first-include-execute-file','2001-07-13');

\i ./postmodern/tests/tef-11.sql

  insert into company_employees (id,name,age,address,include_file,join_date) values (3, 'John', 32, 'Toronto', 'first-include-execute-file','2005-07-13');

  \ir   tef-6.sql

  insert into company_employees (id,name,age,address,include_file,join_date) values (6, 'Johanna', 32, 'Berlin', 'first-include-execute-file','2011-03-13');

    \ir tef-3.sql

    insert into company_employees (id,name,age,address,include_file,join_date) values (4, 'Yasmin', 32, 'Mumbai', 'first-include-execute-file','2007-03-13');

  \i ./postmodern/tests/sub1/tef-4.sql

    insert into company_employees (id,name,age,address,include_file,join_date) values (5, 'Susan', 32, 'Vancouver', 'first-include-execute-file','2009-07-13');

\ir ./sub1/tef-5.sql


insert into company_employees (id,name,age,address,include_file,join_date) values (2, 'Ziad', 32, 'Beirut', 'first-include-execute-file','2003-03-13');
