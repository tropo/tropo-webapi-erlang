%%%-------------------------------------------------------------------
%%% File    : tropo_events.erl
%%% Author  : Dominique Boucher <>
%%% Description : 
%%%
%%% Created : 16 May 2011 by Dominique Boucher <>
%%%-------------------------------------------------------------------
-module(tropo_event).

-behaviour(gen_event).
%% API
-export([start_link/0,
         add_default_handler/0,
         set_level/1,
         add_handler/2,
         delete_handler/2,
         session_created/1,
         session_deleted/1,
         error/2,
         info/2]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(LEVELS, [{debug, 0}, {info, 1}, {warning, 2}, {error, 3}]).

-record(state, {
          level %% log level, one of [debug, info, warning, error]
         }).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | {error,Error} 
%% Description: Creates an event manager.
%%--------------------------------------------------------------------
start_link() ->
    Result = gen_event:start_link({local, ?SERVER}),
    tropo_event:add_default_handler(),
    Result.

%%--------------------------------------------------------------------
%% Function: add_handler() -> ok | {'EXIT',Reason} | term()
%% Description: Adds an event handler
%%--------------------------------------------------------------------
add_default_handler() ->
    add_handler(?MODULE, [debug]).

set_level(Level) ->
    gen_event:call(?SERVER, ?MODULE, {setlevel, Level}).
    

add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

session_created(Id) ->
    gen_event:notify(?SERVER, {new, Id}).

session_deleted(Id) ->
    gen_event:notify(?SERVER, {delete, Id}).

error(Pattern, Args) ->
    gen_event:notify(?SERVER, {error, Pattern, Args}).

info(Pattern, Args) ->
    gen_event:notify(?SERVER, {info, Pattern, Args}).


%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%--------------------------------------------------------------------
init([Level]) ->
    {ok, #state{level = Level}}.

%%--------------------------------------------------------------------
%% Function:  
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% Description:Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event. 
%%--------------------------------------------------------------------
handle_event({new, Pid}, State) ->
    out_msg(State, info, "Session created: ~p", [Pid]),
    {ok, State};
handle_event({delete, Pid}, State) ->
    out_msg(State, info, "Session deleted: ~p", [Pid]),
    {ok, State};
handle_event({error, Pattern, Args}, State) ->
    out_msg(State, error, Pattern, Args),
    {ok, State};
handle_event({info, Pattern, Args}, State) ->
    out_msg(State, debug, Pattern, Args),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_call(Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1, 
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}
%% Description: Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event 
%% handler to handle the request.
%%--------------------------------------------------------------------
handle_call({setlevel, Level}, State) ->
    case lists:keymember(Level, 1, ?LEVELS) of
        true ->
            {ok, ok, State#state{level = Level}};
        false ->
            {ok, {error, "Invalid log level"}, State}
    end;
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler
%% Description: This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description:Whenever an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and 
%% do any necessary cleaning up. 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState} 
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

out_msg(State, Level, Pattern, Args) ->
    case can_output(Level, State#state.level) of
        true ->
            Msg = io_lib:format(Pattern, Args),
            io:format("[~s] [~4..0B/~2..0B/~2..0B ~2..0B:~2..0B:~2..0B] ~s~n", 
                      [level_prefix(Level)] 
                      ++ tuple_to_list(erlang:date()) 
                      ++ tuple_to_list(erlang:time())
                      ++ [Msg]);
        _ ->
            ignore
    end.


level_prefix(error) -> "ERROR";
level_prefix(info) -> "INFO";
level_prefix(warning) -> "WARNING";
level_prefix(debug) -> "DEBUG".

can_output(MessageLevel, HandlerLevel) ->
    {_, MessageIndex} = lists:keyfind(MessageLevel, 1, ?LEVELS),
    {_, HandlerIndex} = lists:keyfind(HandlerLevel, 1, ?LEVELS),
    MessageIndex >= HandlerIndex.
    
