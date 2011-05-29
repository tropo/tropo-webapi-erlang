%%%-------------------------------------------------------------------
%%% File    : tropo_session_sup.erl
%%% Author  : Dominique Boucher <>
%%% Description : 
%%%
%%% Created : 18 May 2011 by Dominique Boucher <>
%%%-------------------------------------------------------------------
-module(tropo_session_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Module, Session) ->
    supervisor:start_child(?SERVER, [Module, Session]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    AChild = {tropo_session, {tropo_session, start_link, []},
              temporary, brutal_kill, worker, [tropo_session]},
    {ok, {{simple_one_for_one, 0, 1}, [AChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
