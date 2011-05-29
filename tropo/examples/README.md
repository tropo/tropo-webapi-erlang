Build and test the example
==========================

To build the examples, your first need to build the Tropo application first.
Then, run the following command at the shell:

    % erl -pa ../ebin -make

You are now ready to test your application. Suppose the `mochiweb` library is installed
in $MOCHI_DIR, you run the application by evaluating the following commands at the
Erlang prompt:

    % erl -pa ../ebin -pa $MOCHI_DIR/ebin 
    Erlang R14B (erts-5.8.1) [source] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]
    
    Eshell V5.8.1  (abort with ^G)
    1> application:start(tropo).
    ok
    2> helloworld:start_link().
    {ok, <0.42.0>}
    3>

and then in another shell, test your application using curl:

    % curl -X POST http://localhost:8000/helloworld/start.json -d '{"session":{"id":"test"}}'
    {"tropo":[{"say":{"value":"Hello, world!"}},{"hangup":null}]}

At the Erlang shell, you should see logging information like

    [INFO] [2011/05/28 21:22:04] Session created: <<"test">>
    [INFO] [2011/05/28 21:22:04] Session deleted: <<"test">>


      
 
