-module(tendon_provider).

-behavior(task_graph_runner).

-include("tendon_int.hrl").

-export([ make_task/4
        , run_task/3
        , guard/3
        ]).

%%%===================================================================
%%% Callback definitions
%%%===================================================================

%% @doc Return initial set of tasks
-callback seed() -> tendon_core:digraph().

%% @doc Execute task
-callback execute( TaskId       :: tendon_core:task_id()
                 , Data         :: term()
                 , GetDepResult :: task_graph_runner:get_deps_result()
                 ) -> task_graph_runner:run_result().

%% @doc Check if task should be executed
-callback guard( TaskId         :: tendon_core:task_id()
               , Data           :: term()
               , GetDepResult   :: task_graph_runner:get_deps_result()
               ) -> task_graph_runner:guard_result().

-callback trigger( tendon_core:task_id()
                 ) -> tendon_core:digraph().

-optional_callbacks([trigger/1]).

%%%===================================================================
%%% task_graph_runner callbacks
%%%===================================================================

run_task(Id, #td_task{provider = Prov, data = Data}, GetDep) ->
  ?set_process_metadata(#{domain => [tendon, provider, Prov, task]}),
  Prov:execute(Id, Data, GetDep).

guard(Id, #td_task{provider = Prov, data = Data}, GetDep) ->
  ?set_process_metadata(#{domain => [tendon, provider, Prov, guard]}),
  Prov:guard(Id, Data, GetDep).

%%%===================================================================
%%% API
%%%===================================================================

-spec make_task( tendon_plugin:provider()
               , tendon_core:task_id()
               , term()
               , [task_graph:resource_id()]
               ) -> task_graph:task(tendon_core:task()).
make_task(Provider, Id, Data, Resources) ->
  #tg_task{ id = Id
          , execute = ?MODULE
          , data = #td_task{ provider = Provider
                           , data     = Data
                           }
          , resources = [jobs | Resources]
          }.
