-module(tendon_event).

-include("tendon.hrl").

%% API
-export([ start_link/1
        , stop/1
        , add_handler/1
        %% , event/3
        %% , event/4
        %% , event/5
        , event_level_to_int/1
        ]).

-define(SERVER, ?MODULE).

-type level() :: debug | info | notice | warning | error | critical.

-reflect_type([level/0]).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Handlers) ->
  Ret = gen_event:start_link({local, ?SERVER}),
  lists:foreach(fun add_handler/1, Handlers),
  Ret.

stop(EventMgr) ->
  gen_event:stop(EventMgr).

add_handler({Module, Args}) ->
  gen_event:add_handler(?SERVER, Module, Args);
add_handler(Module) when is_atom(Module) ->
  gen_event:add_handler(?SERVER, Module, []).

-spec event_level_to_int(level()) -> integer().
event_level_to_int(debug)     -> 100;
event_level_to_int(info)      -> 6;
event_level_to_int(notice)    -> 5;
event_level_to_int(warning)   -> 4;
event_level_to_int(error)     -> 3;
event_level_to_int(critical)  -> 2;
event_level_to_int(alert)     -> 1;
event_level_to_int(emergency) -> 0.
