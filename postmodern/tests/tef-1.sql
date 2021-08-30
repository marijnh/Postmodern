/* test' comment tef-1-1
with multiple lines
  --*/

  insert into company_employees (id,name,age,address,salary,join_date) values (7, 'robert', 32, 'Paris', 20000.00,'2011-04-13');

/*
\i tef-2.sql
*/

\i tef-1.sql
