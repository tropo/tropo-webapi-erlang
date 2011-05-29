%% Author: dboucher
%% Created: Mar 31, 2011
%% Description: TODO: Add description to gen_tropo
-module(gen_tropo).

%%
%% Include files
%%

-include("tropo.hrl").

%%
%% Exported Functions
%%
-export([behaviour_info/1]).
-export([start_link/2]).
-export([handle_req/1]).

%%
%% API Functions
%%

behaviour_info(_) ->
    [{init, 1}, {handle_result, 3}, {terminate, 1}].


start_link(Ip, Port) ->
    %% TODO: We should register the module name somewhere to prevent
    %% converting lists to atoms at runtime.
    mochiweb_http:start([{loop, {?MODULE, handle_req}}, {ip, Ip}, {port, Port}]).

handle_req(Req) ->
    handle_req(Req, Req:get(method), Req:get(path)).

handle_req(Req, 'POST', Path) ->
    TropoReq = tropo:request(Req:recv_body()), 
    [ModuleName| PathInfo] = string:tokens(Path,"/"), 
    Module = list_to_atom(ModuleName), 

    case TropoReq of
        Session = #tropo_session{} ->
            {ok, Pid} = tropo_session:create(Module, Session),
            Result = initial;
        Result = #tropo_result{session_id = SessionId} ->
            {ok, Pid} = tropo_session:get(SessionId)
    end,

    case tropo_session:handle_result(Pid, PathInfo, Result) of
        {ok, Actions} ->
            Response = Actions;
        {stop, Actions} ->
            Response = Actions ++ [tropo:hangup()];
        {error, Reason} ->
            tropo_event:error("An error occurred in Tropo app: ~p", [Reason]),
            Response = [tropo:say(<<"An error occurred.">>), tropo:hangup()]
    end,
    Req:ok({"application/json", [], tropo:response(Response)});


handle_req(Req, Method, Path) ->
    tropo_event:info("Request to unknown resource (~p : ~p)", [Method, Path]),
    Req:respond({404,[],[]}).
	

