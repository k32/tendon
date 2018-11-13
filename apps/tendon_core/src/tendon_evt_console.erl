-module(tendon_evt_console).

-behaviour(gen_event).

-include_lib("task_graph/include/task_graph.hrl").
-include("tendon.hrl").

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-record(s,
        { level                     :: non_neg_integer()
        , num_planned = 0           :: non_neg_integer()
        , num_complete = 0          :: non_neg_integer()
        }).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================
init([]) ->
  Level0 = lee_server:get_d([verbosity]),
  Level = tendon_event:event_level_to_int(Level0),
  {ok, #s{ level = Level
         }}.

handle_event( #tg_event{ kind = add_tasks
                       , data = Data
                       }
            , State0 = #s{num_planned = N}
            ) ->
  State = State0#s{num_planned = length(Data) + N},
  {ok, State};
handle_event( #tg_event{kind = complete_task}
            , State0 = #s{num_complete = N}
            ) ->
  State = State0#s{num_complete = 1 + N},
  {ok, State};
%% handle_event( #td_event{ provider = Provider
%%                        , kind = Kind
%%                        , message = Msg
%%                        , data = Data
%%                        , level = EvtLevel
%%                        , raw_output = RawOutput0
%%                        }
%%             , State = #s{ num_planned = NumPlanned
%%                         , num_complete = NumComplete
%%                         , level = Level
%%                         }
%%             ) when is_integer(EvtLevel)
%%                  , EvtLevel =< Level ->
%%   DataStr = maps:foldl( fun(K, V, Acc) ->
%%                             [io_lib:format("~w:~w", [K, V])|Acc]
%%                         end
%%                       , []
%%                       , Data
%%                       ),
%%   RawOutput =
%%     case RawOutput0 of
%%       [] ->
%%         RawOutput0;
%%       _ ->
%%         [$\n|RawOutput0]
%%     end,

%%   io:format( "[~4.b/~4.b] (~s) ~s ~s~s~n"
%%            , [ NumComplete
%%              , NumPlanned
%%              , tendon_provider:name(Provider)
%%              , Message
%%              , DataStr
%%              , RawOutput
%%              ]
%%            ),
%%   {ok, State};
handle_event(_, State) ->
  {ok, State}.

handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
