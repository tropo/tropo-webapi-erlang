%%% File    : tropo.hrl
%%% Author  : Dominique Boucher <>
%%% Description : Tropo data structures
%%% Created : 28 Feb 2011 by Dominique Boucher <>

-record(tropo_session, {
   account_id,
   session_id,
   call_id,
   headers,
   initial_text,
   parameters,
   timestamp,
   to,
   from,
   user_type
}).

-record(tropo_result, {
   session_id,
   call_id,
   state,
   session_duration,
   sequence,
   complete,
   error,
   actions = []
}).