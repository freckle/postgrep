# PostGrep: A library to parse PostgreSQL logs

[![CircleCI](https://circleci.com/gh/frontrowed/postgrep.svg?style=svg)](https://circleci.com/gh/frontrowed/postgrep)

PostGrep is a collection of Haskell libraries to parse and interpret PostgreSQL
logs. The goal of PostGrep is to provide a way for users to easily create
visualizations of their logs, and also to expose library components that allow
users to create their own log parsing system.

Currently, postgrep is composed of 3 packages:

* `postgrep-core`: This library implements the basic functionality of
  converting a log into a list of `LogEntry`s.
* `postgrep-conduit`: This very small and simple library exposes functions that
  should be used with the `conduit` library. This is the recommended way of
  parsing logs. It is broken off so `postgrep-core` doesn't have extra
  dependencies.
* `postgrep-gcharts`: This package provides a user-facing executable that
  parses a log and visualizes the contents with Google Charts. It is also
  intended to act as a library so users can build their own visualizations.

## Usage

For each of these libraries, the only things required by the user should be a
file or `ByteString` with their log, and the
[`log_line_prefix`](https://www.postgresql.org/docs/9.6/static/runtime-config-logging.html#GUC-LOG-LINE-PREFIX)
format for their logs. First, postgrep parses the `log_line_prefix` into a
regex-based parser. This is then used to parse log lines and group them
together into log entries.

## TODO

* Handle syslog and syslog2 lines. So far we only handle stderr style logs.
