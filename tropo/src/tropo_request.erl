%% Author: dboucher
%% Created: Mar 31, 2011
%% Description: TODO: Add description to tropo_request
-module(tropo_request, [Session, Result]).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([get/1]).

%%
%% API Functions
%%

get(session) -> Session;

get(result) -> Result.


%%
%% Local Functions
%%

