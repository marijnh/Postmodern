create table company_employees(
   id bigserial primary key     not null,
   name           text    not null,
   age            int     not null,
   address        char(50),
   salary         real,
   join_date	  date
);
-- Test comment 1
insert into company_employees (id,name,age,address,salary,join_date) values (1, 'paul', 32, 'London', 20100.00,'2001-07-13');
insert into company_employees (id,name,age,address,salary,join_date) values (2, 'ziad', 32, 'Beirut', 20000.00,'2003-03-13');
/* test comment 2
with multiple lines
*/
insert into company_employees (id,name,age,address,salary,join_date) values (3, 'john', 32, 'Toronto', 20100.00,'2005-07-13');
insert into company_employees (id,name,age,address,salary,join_date) values (4, 'yasmin', 32, 'Mumbai', 20000.00,'2007-03-13');
/* test comment 3 (asterisk in second line of multiline comment)
 * with multiple lines
*/
insert into company_employees (id,name,age,address,salary,join_date) values (5, 'susan', 32, 'Vancouver', 20100.00,'2009-07-13');
insert into company_employees (id,name,age,address,salary,join_date) values (6, 'johanna', 32, 'Berlin', 20000.00,'2011-03-13');
