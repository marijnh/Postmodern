begin transaction;

create table company_employees(
   id bigserial primary key     not null,
   name           text    not null,
   age            int     not null,
   address        char(50),
   salary         real,
   join_date	  date
);
insert into company_employees (id,name,age,address,salary,join_date) values (1, 'paul', 32, 'London', 20100.00,'2001-07-13');
insert into company_employees (id,name,age,address,salary,join_date) values (1, 'paul', 32, 'London', 20100.00,'2001-07-13');
insert into company_employees (id,name,age,address,salary,join_date) values (2, 'ziad', 32, 'Beirut', 20000.00,'2003-03-13');
end transaction;
