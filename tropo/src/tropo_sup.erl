%%% File    : tropo_sup.erl
%%% Author  : Dominique Boucher <>
%%% Description : Main supervisor for the Tropo application
%%% Created : 18 May 2011 by Dominique Boucher <>

-module(tropo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, 
           [
            ?CHILD(tropo_event, worker),
            ?CHILD(tropo_server, worker),
            ?CHILD(tropo_session_sup, worker)
           ]} }.

