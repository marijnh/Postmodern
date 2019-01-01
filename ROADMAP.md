# Roadmap
Postmodern is a common lisp support library for the postgresql database. It makes
no attempt to be database agnostic. You can think of postmodern as having three components
- cl-postgres : a low level interface for communicating with a postgresql database server
- s-sql : a lispy dsl providing possibly a more comfortable way to write sql queries, and
- postmodern : a package which extends the cl-postgres interface. Comments are solicited on
  whether the cl-postgres and postmodern packages should be combined.

This draft longterm roadmap is a work in progress on issue resolution and additional
functionality under consideration. Requests for different priorities are expected and
welcomed, particularly by anyone willing to work on the item.

No guarantee is given with respect to resolution or timing on any item.

## Sql support
- [X]   Aggregate operator support - order-by, filter, distinct. This resolves issue 68
        and adds functionality from postgresql 9.5
- [X]   Within group clause for ordered set aggregates - (mode, percentile-cont, percentile-disc. Postgresql 9.4)
- [X]   Statistical operators (corr, covar-pop, covar-samp, regr-avgx, regr-avgy, regr-count,
        regr-intercept, regr-r2, regr-slope, regr-sxx, regr-sxy, regr-syy, stddev, stddev-pop,
        stddev-samp, variance, var-pop, var-samp)
- [X]   Grouping-sets (postgresql 9.5)
- [X]   Values Clause
- [ ]   Hypothetical Set Aggregates Functions (rank, dense-rank, percent-rank, cume-dist)
- [X]   Empty-Set - Add the ability for the set operator to return "()" instead of "(NULL)" or "false".
        This is a current work-around using the def-sql-op :empty-set which is overly verbose. Better ideas would be welcomed.
- [ ]   UUID (see e.g  https://github.com/michaeljforster/cl-postgres-plus-uuid)
        Postgresql has a uuid extension. A database owner needs to add the extension manually to the specific database, calling:
        create extension if not exists "uuid-ossp";
        A uuid can then be generated in postmodern by calling (query (:select (:uuid-generate-v1)))
- [ ]   Generate-Series needs testing and interval work
- [X]   Extract needs testing
- [ ]   Lateral Join (postgresql 9.3)
- [X]   Identity columns (postgresql 10)
- [ ]   Transition tables for triggers (postgresql 10)
- [ ]   Hash Indexes (postgresql 10, See https://blog.2ndquadrant.com/postgresql-10-identity-columns/,
        https://www.depesz.com/2017/04/10/waiting-for-postgresql-10-identity-columns/)
- [X]   Insert on conflict do nothing/update (postgresql 9.5)
- [ ]   Full text search with phrases (postgresql 9.6)
- [ ]   WITH CHECK clause - Auto-updatable views can now specify whether an INSERT or UPDATE
        would change the state of the row so that it would no longer be visible in the view.
        Using WITH CHECK OPTION will prevent any such changes from occuring. (postgresql 9.4)
- [ ]   WITH ORDINALITY clause (postgresql 9.4)
- [ ]   Table Creation with different indexes (various postgresql version additions)
- [ ]   Postgresql regular expression support - see https://www.postgresql.org/docs/current/static/pgtrgm.html
- [ ]   Multiple row upserts

## Data type support
- [ ]   json, jsonb (postgresql 9.4, full text search support in postgresql 10) See
        https://github.com/gtod/postgres-json/blob/master/postgres/s-sql.lisp
        Include exporting to json strings acceptable to the different common lisp libraries
- [X]   Array support (see issue 121)
- [X]   Intervals (see e.g. issue 104)
- [ ]   Ranges
- [ ]   Postgis
- [ ]   XML (see also xmltable in postgresql 10)
- [ ]   Enum needs testing and documentation
- [ ]   Network Addresses
- [ ]   Others?

## DAO Support
- [X]   Fix issue 70 - build-dao-methods fails when a slot is named "from".
- [ ]   Review errors with *ignore-unknown-columns*. Sometimes hangs without raising an error.
- [ ]   Type validation - make it easier to use col-type to ensure that a slot value is what the database expects
- [X]   Document export to dao similar to :alist, alists, plist, plists
- [ ]   Add foreign key support to dao, both columns and as a whole, similar to s-sql support

## Other Support
- [ ]   Migration Support (compare with https://pypi.org/project/alembic/)
- [ ]   Named Prepared Statement explicit arglist
- [ ]   SQL Read Table Review (comments requested on any work that should be done here)
- [ ]   Row Reader Review (comments requested on any work that should be done here)
- [ ]   Prepared Query Review (comments requested on any work that should be done here)
- [ ]   Reading large bytea column over ssl connection errors have been reported. Postgresql does not
        have a chunk API so the network is handling the content as a whole.
- [ ]   SCRAM authentication (postgresql 10)
- [ ]   Alter system (postgresql 9.4)
- [X]   Create temp tables

## Connections/Reconnections and Transactions
- [ ]   Ensure transactions can deal with reconnections/restarts
- [X]   Ensure prepared statements and reconnect restart work together
- [ ]   IPV6 connections
- [X]   Expand transaction isolation. Currently with-transaction does not allow specifying the isolation level.
        See https://www.postgresql.org/docs/current/static/transaction-iso.html

## Error Messages
- [X]   Prepared statement error messages. Add number of parameters expected, incorrect number of parameters given
- [X]   write-ratio-as-floating-point [rational] See proposed Attila Lendvai commit

## Conditionality and composition
- [X]   Document ways to generate dynamic queries from sql fragments. Issue 127 and 111

## Date and Time Sanity
- [X]   Simple-Date manual loading documented
- [X]   Local-time manual loading documented
- [X]   Intervals documented

## Documentation
- [ ]   Setup
- [ ]   Creating databases and users
- [ ]   Usage examples for sql users
- [ ]   Usage examples for non-sql users
- [X]   More examples of create table
- [ ]   Creation of new datatypes
- [ ]   Pooling with external pooling applications

## Security Audit

## Long Range
- [ ]   Consider extending dao into more ORM capability
- [ ]   Cluster Support
- [ ]   Replication Support

## Separate Project - Monitoring Application
