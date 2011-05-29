%%%-------------------------------------------------------------------
%%% File    : tropo_session.erl
%%% Author  : Dominique Boucher <>
%%% Description : Tropo Session 
%%%
%%% Created : 17 May 2011 by Dominique Boucher <>
%%%-------------------------------------------------------------------
-module(tropo_session).

-behaviour(gen_server).

%% API
-export([create/2, handle_result/3, get/1, stop/1]).


%% used by the supervisor
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define(DEFAULT_TIMEOUT, 120 * 1000).

-record(state, {module, session, app_state}).

-include("tropo.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Module, Session) ->
    gen_server:start_link(?MODULE, [Module, Session], []).


create(Module, Session) ->
    tropo_session_sup:start_child(Module, Session).


handle_result(Pid, Path, Result) ->
    gen_server:call(Pid, {handle_result, Path, Result}).

get(SessionId) ->
    tropo_server:get_session(SessionId).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Module, Session = #tropo_session{session_id = SessionId}]) 
  when is_atom(Module) ->
    ok = tropo_server:new_session(SessionId, self()),
    tropo_event:session_created(SessionId),
    {ok, State} = Module:init(Session),
    {ok, #state{module = Module, session = Session, app_state = State}, ?DEFAULT_TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({handle_result, Path, Result}, _From, State) ->
    Module = State#state.module,
    Tropo = tropo_request:new(State#state.session, Result),
    case Module:handle_result(Tropo, Path, State#state.app_state) of
        {ok, Actions, NewState} ->
            {reply, {ok, Actions}, State#state{app_state = NewState}, ?DEFAULT_TIMEOUT};
        {stop, Actions} ->
            {stop, normal, {stop, Actions}, State};
        {error, Reason} ->
            {stop, error, {error, Reason}, State}
    end;
handle_call(Request, _From, State) ->
    tropo_event:info("Invalid call message : ~p", [Request]),    
    {reply, ok, State, ?DEFAULT_TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Msg, State) ->
    tropo_event:info("Invalid cast message : ~p", [Msg]),    
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    tropo_event:info("Session timeout for session ~p", [session_id(State)]),
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    SessionId = session_id(State),
    tropo_server:remove_session(SessionId),
    tropo_event:session_deleted(SessionId),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


session_id(State) ->
    State#state.session#tropo_session.session_id.
