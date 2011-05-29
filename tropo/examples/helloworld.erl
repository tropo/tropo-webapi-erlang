%%% File    : helloworld.erl
%%% Author  : Dominique Boucher <>
%%% Description : Tropo webapi example
%%% Created : 23 May 2011 by Dominique Boucher <>

-module(helloworld).


-behaviour(gen_tropo).
-include("tropo.hrl").

-export([start_link/0, init/1, handle_result/3, terminate/1]).


%% Starts the tropo application server.
%% The Tropo application must be configured to request voice documents
%% from this URL:
%%   http://yourhostname:8000/helloworld/start.json
start_link() ->
    gen_tropo:start_link("127.0.0.1", 8000).


%% Initializes a tropo session with the session data.
%% The session state is 'none' as we don't need any state.
init(_TropoSession) ->
    {ok, none}.

%% Handles a request from Tropo.
%% The first argument is ignored as we don't need the session data
%% nor the interaction result.
handle_result(_Tropo, ["start.json"], _State) ->
    Actions = [tropo:say(<<"Hello, world!">>, [])],
    %% Stop the session after playing the prompt. This will automatically
    %% call 'hangup' on the Tropo side.
    {stop, Actions}.

terminate(_State) ->
    ok.





