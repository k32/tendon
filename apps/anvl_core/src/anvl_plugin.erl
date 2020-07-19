-module(anvl_plugin).

-include("anvl.hrl").

-export([ builtin_plugins/0
        , providers/1
        , seed/1
        , plugins/0
        ]).

%%%===================================================================
%%% Type definitions
%%%===================================================================

-type provider() :: module().

-type plugin() :: anvl_core:app_id().

-reflect_type([plugin/0, provider/0]).

%%%===================================================================
%%% Callback definitions
%%%===================================================================

%% @doc Returns model of the global configuration:
-callback model() -> lee:module().

%% @doc Returns model of the project configuration:
-callback project_model() -> lee:module().

%% @doc Return a list of providers defined in the plugin:
-callback providers() -> [provider()].

%%%===================================================================
%%% API functions
%%%===================================================================

-spec builtin_plugins() -> [plugin()].
builtin_plugins() ->
  [anvl_core, anvl_compile].

-spec providers(plugin()) -> [provider()].
providers(Plugin) ->
  Plugin:providers().

-spec seed(plugin()) -> anvl_core:digraph().
seed(Plugin) ->
  Seeds = [P:seed() || P <- providers(Plugin)],
  anvl_lib:merge_digraphs(Seeds).

%% @doc List available plugins
-spec plugins() -> [plugin()].
plugins() ->
  %% TODO
  builtin_plugins().
