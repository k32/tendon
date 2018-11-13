-module(tendon_core).

-behavior(tendon_provider).

-include("tendon_int.hrl").

-export([ main/1
        , name/0
        , options/0
        ]).

-export_type([ provider/0
             ]).

-type provider() :: atom().

-type toplevel_target() :: {atom(), }

%% Logging callbacks:
-export([log_level/1, log_pretty_format/1]).

name() ->
  "tendon".

description() ->
  "uwu".

usage() ->
  "".

options() ->
  #{ base_dir =>
       fun tendon_provider:is_string/1
   , root_dir =>
       fun tendon_provider:is_string/1
     %% Should not clash with rebar3 plugins options...
   , providers_dir =>
       fun tendon_provider:is_string/1
   }.

main(Opts) ->
  EventMgr = tendon_event:start_link(),
  try
    tendon_main(EventMgr)
  catch
    _:Err ?BIND_STACKTRACE(Stack) ->
      ?GET_STACKTRACE(Stack),
      ?INTERNAL_ERROR( "Uncaught exception"
                     , #{ error => Err
                        , stacktrace => Stack
                        }
                     )
  after
    tendon_event:stop(EventMgr)
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

tendon_main(EventMgr) ->
  Settings0 = #{event_manager => EventMgr},
  Settings = Settings0,
  Graph = build_graph(),
  case task_graph:run_graph(?MODULE, Settings, Graph) of
    {ok, Result} ->
      make_success(Result);
    {error, Error} ->
      make_error(Error)
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
