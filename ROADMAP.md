# Possible Roadmap
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
- [ ]   UUID (see e.g  https://github.com/michaeljforster/cl-postgres-plus-uuid)
        Postgresql has a uuid extension. A database owner needs to add the extension manually to the specific database, calling:
        create extension if not exists "uuid-ossp";
        A uuid can then be generated in postmodern by calling (query (:select (:uuid-generate-v1)))
- [ ]   Transition tables for triggers (postgresql 10)
- [ ]   Hash Indexes (postgresql 10, See https://blog.2ndquadrant.com/postgresql-10-identity-columns/,
        https://www.depesz.com/2017/04/10/waiting-for-postgresql-10-identity-columns/)
- [ ]   Full text search with phrases (postgresql 9.6 and additional functionality in 12.0)
- [ ]   WITH CHECK clause - Auto-updatable views can now specify whether an INSERT or UPDATE
        would change the state of the row so that it would no longer be visible in the view.
        Using WITH CHECK OPTION will prevent any such changes from occuring. (postgresql 9.4)
- [ ]   Table Creation with different indexes (various postgresql version additions)
- [ ]   Generated columns - see https://pgdash.io/blog/postgres-12-generated-columns.html
- [ ]   Postgresql regular expression support - see https://www.postgresql.org/docs/current/static/pgtrgm.html
- [ ]   Create table by selecting from another table.

## Data type support
- [ ]   json, jsonb (postgresql 9.4, full text search support in postgresql 10) See
        https://github.com/gtod/postgres-json/blob/master/postgres/s-sql.lisp
        Include exporting to json strings acceptable to the different common lisp libraries
- [ ]   Ranges
- [ ]   Postgis
- [ ]   XML (see also xmltable in postgresql 10)
- [ ]   Enum needs testing and documentation
- [ ]   Network Addresses
- [ ]   Others?

## DAO Support
- [ ]   Review errors with *ignore-unknown-columns*. Sometimes hangs without raising an error.
- [ ]   Type validation - make it easier to use col-type to ensure that a slot value is what the database expects

## Other Support
- [ ]   Migration Support (compare with https://pypi.org/project/alembic/)
- [ ]   Named Prepared Statement explicit arglist
- [ ]   SQL Read Table Review (comments requested on any work that should be done here)
- [ ]   Row Reader Review (comments requested on any work that should be done here)
- [X]   Allow parameters to be passed as binary to postgresql

## Connections/Reconnections and Transactions
- [ ]   Ensure transactions can deal with reconnections/restarts
- [ ]   IPV6 connections

## Documentation
- [ ]   Setup
- [ ]   Usage examples for sql users
- [ ]   Usage examples for non-sql users
- [ ]   Creation of new datatypes
- [ ]   Pooling with external pooling applications

## Armed Bear Issues
- [ ]   What is the problem with :null
- [ ]   Armed Bear issues with unicode. See e.g. icelandic cities in test-s-sql

## Security Audit

## Long Range (Likely Never)
- [ ]   Consider extending dao into more ORM capability
- [ ]   Multi-Cluster Support
- [ ]   Replication Support
