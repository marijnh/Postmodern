# Contributing to Postmodern
Thank you for potentially  contributing to Postmodern. We have tried to set out a few guidelines below to help both you and the maintainers.

# Bugs and Enhancements
We welcome new issue submissions identifying bugs and requested enhancements. We also welcome bug fixes and proposed code for enhancements. If you have found a security bug, please email the current maintainer shown in the asd files directly rather than creating new issue on github.

Please clearly describe the issue and test cases are very much appreciated. Proposed code changes and additions should, at a minimum, have a doc string for any new function or appropriate modification of existing doc string. We prefer that documentation also be added to the appropriate doc org file and tests using fiveam be added to the appropriate testing file (each major sub-package - see below - has its own testing directory). You do not need to modify the html doc files because those will be automatically updated from the org files. Note that the docs in the repository are current, the docs at http://marijnhaverbeke.nl/postmodern are not necessarily up to date.

We also would appreciate it if you note which common lisp implementations and operating system you have used to test and ensure that your bugfix or enhancement actually works.

As a reminder - because we have done this ourselves - test that the new version can be compiled from a clean start (e.g. no debugging statements remain from a logging library).

We are always open to more tests, regardless of whether you have found a bug or need an enhancement.

# Code of Conduct
We have not had any historical issues with conduct related to postmodern, its users or contributors. That being said, we expect that people will behave with respect towards users or contributors. This is all volunteer work by everyone and while the range of experience and expertise varies widely, harrassment or insults do not add value to the code or the community and will not be tolerated.

# Licensing
All code is under the license provided in the LICENSE file found in the root directory. If you want to contribute existing code which has another license, you need to get permission from that licensor to have the code released under the same rules as the rest of postmodern or work with the maintainer(s) for appropriate licensing.

# Questions
Feel free to email the current maintainer as shown in the asd files if you want to address something off line.

# The structure of Postmodern
Postmodern has four main packages - cl-postgres, s-sql, postmodern and simple-date. There are a few things to keep in mind here:

## cl-postgres
cl-postgres establishes the network connections with postgresql and is used by other libraries besides postmodern. Any changes to cl-postgres functionality or api needs to be thoroughly tested and documented.

## postmodern
The postmodern package adds the querying structures and utility functions to the networking connections provided by cl-postgres

## s-sql
s-sql is a lispy wrapper around the sql language. You do not need to use s-sql to use postmodern.

## simple-date
simple-date is an add-on date library. simple-date is not loaded automatically and many people use local-time instead. Do not assume that simple-date will be loaded by any user.
