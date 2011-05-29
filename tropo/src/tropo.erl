%% Author: dboucher
%% Created: Mar 29, 2011
%% Description: TODO: Add description to tropo_webapi
-module(tropo).

%%
%% Include files
%%
-include("tropo.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%
%% Exported Functions
%%
-export([ask/3, call/2, conference/2, hangup/0, 
         message/3, on/2, record/1, redirect/2, 
         reject/0, say/2, start_recording/1,
         stop_recording/0, transfer/2]).

-export([start_session/2, start_session/3]).

-export([request/1, response/1]).

%%
%% API Functions
%%

call(Destination, Args) ->
    {struct, 
     [{<<"call">>, 
       {struct, convert_values([{<<"to">>, Destination} | Args])}}]}.

say(Value, Args) ->
    {struct, 
     [{<<"say">>, 
       {struct, [{<<"value">>, convert_value(Value)} | convert_values(Args)]}}]}.

ask({struct, [Say]}, Choices, Options) ->
    {struct,
     [{<<"ask">>,
       {struct, [Say,
		 {<<"choices">>,
		  {struct, [{<<"value">>, convert_value(Choices)}]}}
		| convert_values(Options)]}}]}.

message({struct, [Say]}, To, Options) ->
    {struct,
     [{<<"message">>,
       {struct, [Say, 
                 {<<"to">>, convert_value(To)}
                 | convert_values(Options)]}}]}.

conference(Id, Options) ->
    {struct,
     [{<<"conference">>,
       {struct, [{<<"id">>, convert_value(Id)}
                 | convert_values(Options)]}}]}.

record(Options) ->
    {struct,
     [{<<"record">>,
       {struct, convert_values(Options)}}]}.

redirect(To, Options) ->
    {struct,
     [{<<"redirect">>,
       {struct, [{<<"to">>, convert_value(To)}
                 | convert_values(Options)]}}]}.

reject() ->
    {struct, [{<<"reject">>, null}]}.

transfer(To, Options) ->
    {struct,
     [{<<"transfer">>,
       {struct, [{<<"to">>, convert_value(To)}
                 | convert_values(Options)]}}]}.

start_recording(Options) ->
    {struct,
     [{<<"startRecording">>,
       {struct, convert_values(Options)}}]}.

stop_recording() ->
    {struct, [{<<"stopRecording">>, null}]}.

on(Event,Next) ->
    {struct,
     [{<<"on">>,
       {struct,
        [{<<"event">>,convert_value(Event)},
          {<<"next">>,convert_value(Next)}]}}]}.

hangup() ->
    {struct, [{<<"hangup">>, null}]}.


%%
%% Session API
%%

start_session(Token, Params) ->
    start_session(Token, Params, 10000).

start_session(Token, Params, Timeout) ->
    HttpParams = http_params([{<<"action">>,<<"create">>}, {<<"token">>,Token}]++Params), 
    Response = httpc:request(get,
                             {"https://api.tropo.com/1.0/sessions?"++HttpParams,[]},
                             [{timeout,Timeout},{connect_timeout,Timeout}],
                             []), 
    case Response of
        {ok,{{_,200,_},_Headers,Body}} ->
            case success_value(Body) of
                "true" ->
                    ok;
                _Else ->
                    {error,"Unknown error"}
            end;
        {error,Reason} ->
            {error,Reason}
    end.
    

%%
%% Some helpers for the http front-end
%% 

request(Body) ->
    {struct, [{Type, {struct, Properties}}]} = mochijson2:decode(Body),
    parse_request_properties(Type, Properties).

response(Elements) ->
    mochijson2:encode({struct, [{<<"tropo">>, Elements}]}).


%%
%% Local Functions
%%    


convert_values(Args) ->
    [{convert_value(Key), convert_value(Val)} || {Key,Val} <- Args].

convert_value(Bin) when is_binary(Bin) ->
    Bin;
convert_value(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom));
convert_value(List) when is_list(List) ->
    list_to_binary(List);
convert_value(Val) ->
    Val.


parse_request_properties(<<"session">>, Props) ->
    parse_session_props(#tropo_session{}, Props);
parse_request_properties(<<"result">>, Props) ->
    parse_result_props(#tropo_result{}, Props).

parse_session_props(Session, []) ->
    Session;
parse_session_props(Session, [{<<"accountId">>, Id} | Props]) ->
    parse_session_props(Session#tropo_session{account_id = Id}, Props);
parse_session_props(Session, [{<<"callId">>, Id} | Props]) ->
    parse_session_props(Session#tropo_session{call_id = Id}, Props);
parse_session_props(Session, [{<<"from">>, From} | Props]) ->
    parse_session_props(Session#tropo_session{from = From}, Props);
parse_session_props(Session, [{<<"to">>, To} | Props]) ->
    parse_session_props(Session#tropo_session{to = To}, Props);
parse_session_props(Session, [{<<"headers">>, Hdrs} | Props]) ->
    parse_session_props(Session#tropo_session{headers = Hdrs}, Props);
parse_session_props(Session, [{<<"id">>, Id} | Props]) ->
    parse_session_props(Session#tropo_session{session_id = Id}, Props);
parse_session_props(Session, [{<<"initialText">>, Text} | Props]) ->
    parse_session_props(Session#tropo_session{initial_text = Text}, Props);
parse_session_props(Session, [{<<"parameters">>, {struct, Params}} | Props]) ->
    parse_session_props(Session#tropo_session{parameters = Params}, Props);
parse_session_props(Session, [{<<"timestamp">>, TS} | Props]) ->
    parse_session_props(Session#tropo_session{timestamp = TS}, Props);
parse_session_props(Session, [{<<"userType">>, Type} | Props]) ->
    parse_session_props(Session#tropo_session{user_type = Type}, Props);
parse_session_props(Session, [{K, _} | Props]) ->
    error_logger:warning_msg("Invalid Tropo session property (ignored): ~p~n", [K]),
    parse_session_props(Session, Props).

    
parse_result_props(Result, []) ->
    Result;
parse_result_props(Result, [{<<"actions">>, Actions} | Props]) ->
    parse_result_props(Result#tropo_result{actions = Actions}, Props);
parse_result_props(Result, [{<<"callId">>, Id} | Props]) ->
    parse_result_props(Result#tropo_result{call_id = Id}, Props);
parse_result_props(Result, [{<<"sessionId">>, Id} | Props]) ->
    parse_result_props(Result#tropo_result{session_id = Id}, Props);
parse_result_props(Result, [{<<"complete">>, Bool} | Props]) ->
    parse_result_props(Result#tropo_result{complete = Bool}, Props);
parse_result_props(Result, [{<<"error">>, Reason} | Props]) ->
    parse_result_props(Result#tropo_result{call_id = Reason}, Props);
parse_result_props(Result, [{<<"state">>, State} | Props]) ->
    parse_result_props(Result#tropo_result{state = State}, Props);
parse_result_props(Result, [{<<"sessionDuration">>, Length} | Props]) ->
    parse_result_props(Result#tropo_result{session_duration = Length}, Props);
parse_result_props(Result, [{<<"sequence">>, Id} | Props]) ->
    parse_result_props(Result#tropo_result{sequence = Id}, Props);
parse_result_props(Result, [{K, _} | Props]) ->
    error_logger:warning_msg("Invalid Tropo result property (ignored): ~p~n", [K]),
    parse_result_props(Result, Props).


success_value(Body) when is_binary(Body) ->
    success_value(binary_to_list(Body));

success_value(Body) when is_list(Body) ->
    {Dom, _} = xmerl_scan:string(Body),
    [Node|_] = xmerl_xpath:string("success/text()", Dom),
    Node#xmlText.value.


http_params(Params) ->
    string:join(lists:map(fun http_param/1, Params), "&").

http_param({Param, Value}) ->
    edoc_lib:escape_uri(to_string(Param)) ++ "=" ++ edoc_lib:escape_uri(to_string(Value)).

to_string(B) when is_binary(B) ->
    binary_to_list(B);
to_string(L) when is_list(L) ->
    L;
to_string(A) when is_atom(A) ->
    atom_to_list(A).

