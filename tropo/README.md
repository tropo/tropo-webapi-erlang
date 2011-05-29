Tropo Application Server in Erlang
==================================

This Erlang library implements an application server for Tropo apps.

Requirements
------------

The application depends on a single Erlang library, the
[mochiweb](http://github.com/mochi/mochiweb) library for building HTTP
servers.


Compiling
---------

Compilation is done using [rebar](http://github.com/basho/rebar). If
the mochiweb source code is in `$MOCHI_DIR`, run the following command
at the shell prompt:

    % rebar deps_dir=$MOCHI_DIR compile
    ==> mochiweb (compile)
    ...
    ==> tropo (compile)
    ...

Examples
--------

A first set of examples are in the `examples` subdirectory.

Documentation
-------------

To be completed.


