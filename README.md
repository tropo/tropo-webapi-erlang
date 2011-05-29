Tropo webapi in Erlang
======================

This Erlang library implements a Tropo application server with full session handling. It features:

- An Erlang application with supervision tree for easy deployment.
- Tropo session handling and timeout management. Sessions are represented as full-fledged Erlang gen_servers. 
- A behaviour for implementing Tropo applications easily (see the examples directory for sample applications).
- A Tropo-specific event manager. 
- An API to create all Tropo verbs.

What's missing
--------------

- Support for distribution and load-balancing of sessions on multiple Erlang nodes
- Validation of all options passed to Tropo API functions
- Documentation
- Tests!


Author
------

Dominique Boucher, Nu Echo Inc.

