-module(tendon_event).

%% API
-export([ start_link/0
        , stop/1
        , add_handler/0
        , event/3
        , event/4
        , event/5
        ]).

-export_type([ kind/0
             , level/0
             , plugin/0
             ]).

-define(SERVER, ?MODULE).

-type kind() :: atom().

-type level() :: integer().

%%%===================================================================
%%% API
%%%===================================================================
start_link(Handlers) ->
  Ret = gen_event:start_link({local, ?SERVER}),
  lists:foreach(fun add_handler/1, Handlers),
  Ret.

stop(EventMgr) ->
  gen_event:stop(EventMgr).

add_handler(Module) ->
  gen_event:add_handler(?SERVER, Module, []).

-spec event( tendon_core:provider()
           , level()
           , string()
           ) -> ok.
event(Provider, Level, Message) ->
  event(Provider, Level, Message, #{}, []).

-spec event( tendon_core:provider()
           , level()
           , string()
           , #{atom() => term()}
           ) -> ok.
event(Provider, Message, Data) ->
  event(Provider, Level, Message, Data, []).

-spec event( tendon_core:provider()
           , level()
           , string()
           , #{atom() => term()}
           , iolist()
           ) -> ok.
event(Provider, Level, Messagem, Data, RawOutput) ->
  gen_event:notify( ?SERVER
                  , #td_event{ timestamp = erlang:system_time(?tg_timeUnit)
                             , provider = Provider
                             , kind = Kind
                             , data = Data
                             , raw_output = RawOutput
                             }
                  ).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
