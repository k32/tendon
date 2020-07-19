-module(anvl_provider).

-behavior(task_graph_runner).

-include("anvl_int.hrl").

-export([ make_task/4
        , run_task/3
        , guard/3
        ]).

%%%===================================================================
%%% Callback definitions
%%%===================================================================

%% @doc Return initial set of tasks
-callback seed() -> anvl_core:digraph().

%% @doc Execute task
-callback execute( TaskId       :: anvl_core:task_id()
                 , Data         :: term()
                 , GetDepResult :: task_graph_runner:get_deps_result()
                 ) -> task_graph_runner:run_result().

%% @doc Check if task should be executed
-callback guard( TaskId         :: anvl_core:task_id()
               , Data           :: term()
               , GetDepResult   :: task_graph_runner:get_deps_result()
               ) -> task_graph_runner:guard_result().

-callback trigger( anvl_core:task_id()
                 ) -> anvl_core:digraph().

-optional_callbacks([trigger/1]).

%%%===================================================================
%%% task_graph_runner callbacks
%%%===================================================================

run_task(Id, #td_task{provider = Prov, data = Data}, GetDep) ->
  ?set_process_metadata(#{domain => [anvl, provider, Prov, task]}),
  Prov:execute(Id, Data, GetDep).

guard(Id, #td_task{provider = Prov, data = Data}, GetDep) ->
  ?set_process_metadata(#{domain => [anvl, provider, Prov, guard]}),
  Prov:guard(Id, Data, GetDep).

%%%===================================================================
%%% API
%%%===================================================================

-spec make_task( anvl_plugin:provider()
               , anvl_core:task_id()
               , term()
               , [task_graph:resource_id()]
               ) -> task_graph:task(anvl_core:task()).
make_task(Provider, Id, Data, Resources) ->
  #tg_task{ id = Id
          , execute = ?MODULE
          , data = #td_task{ provider = Provider
                           , data     = Data
                           }
          , resources = [jobs | Resources]
          }.
