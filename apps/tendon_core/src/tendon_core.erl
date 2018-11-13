-module(tendon_core).

-behavior(tendon_plugin).

-include("tendon_int.hrl").

-export([ main/1
        , panic/2
        , model/0
        , metamodel/0
        , project_model/0
        , providers/0
        ]).

-export_type([ task/0
             , digraph/0
             ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type profile() :: atom().

-type task() :: #td_task{}.

-type digraph() :: task_graph:digraph(task()).

-type app_id() :: atom().

-type package_id() :: root | {dep, atom()}.

-type props(K, V) :: [{K, V}] | #{K => V}.

-type props() :: props(atom(), term()).

-type overrides() :: [{add, app_id(), props()}].

-type optional(A) :: A | undefined.

-type maybe(A) :: {just, A} | undefined.

%% Use metamodels defined in the following modules:
-define(base_interface_modules, [ lee_cli
                                , lee_consult
                                , lee_os_env
                                , tendon_lib
                                ]).

-reflect_type([ overrides/0, optional/1, maybe/1, app_id/0
              , package_id/0, profile/0
              ]).

%%%===================================================================
%%% Lee models
%%%===================================================================

-define(ns, tendon).

metamodel() ->
  #{ metatype =>
       #{ project_config =>
            {[metatype], #{}}
        }
   }.

%%%===================================================================
%%% tendon_plugin callbacks
%%%===================================================================

-spec project_model() -> lee:module().
project_model() ->
  ProjectCfgModel0 =
    #{ base_dir =>
         {[value, mustache],
          #{ oneliner => "Directory where build artifacts are stored"
           , type => string()
           , default => "_tbuild/{{cfg: [profile]}}"
           , file_key => base_dir
           }}
     , root_dir =>
         {[value, mustache],
          #{ oneliner => "Directory where project files are located"
           , type => string()
           , default => "."
           , file_key => root_dir
           }}
     , checkouts_dir =>
         {[value, mustache],
          #{ oneliner => "Directory where checkouts are located"
           , type => string()
           , default => "_checkouts"
           , file_key => checkouts_dir
           }}
     , app_dirs =>
         {[value, mustache],
          #{ oneliner => "Directories where project applications are located"
           , type => list(string())
           , default => ["apps/*", "lib/*", "."]
           , file_key => project_app_dirs
           }}
     , plugins =>
         {[value],
          #{ oneliner => "List of tendon plugins"
           , type => list(tendon_plugin:plugin())
           , default => []
           , file_key => tendon_plugins
           , doc_remark => "Tendon plugins are incompatible with the rebar3 providers"
           }}
     , overrides =>
         {[value],
          #{ onliner => "TODO: not implemented"
           , type => overrides()
           , default => []
           , file_key => overrides
           }}
     %% , package_id =>
     %%     {[value],
     %%      #{ oneliner => "ID of the project (set automatically)"
     %%       , type => package_id()
     %%       , undocumented => true
     %%       }}
     }.

model() ->
  Projects = #{ project =>
                  {[map]
                  , #{?key_elements => [package_id]}
                  , merged_project_model()
                  }
              },
  {ok, Model} = lee_model:merge([ global_config_model()
                                , Projects
                                ]),
  Model.

providers() ->
  [].

%%%===================================================================
%%% API functions
%%%===================================================================

-spec main([string()]) -> no_return().
main(Opts) ->
  try
    InterfaceModules = ?base_interface_modules ++
      tendon_plugin:plugins(),
    application:set_env(lee, interface_modules, InterfaceModules),
    application:ensure_all_started(tendon_core),
    read_global_config(Opts),
    maybe_show_help_and_exit(),
    start_logging(),
    read_project_config(root, "."),
    case tendon_main(Opts) of
      ok ->
        erlang:halt(0);
      error ->
        erlang:halt(1)
    end
  catch
    exit:{panic, Fmt, Args} ?BIND_STACKTRACE(Stack) ->
      %% Panic is a semi-expected outcome and it is caused by user
      %% error:
      ?GET_STACKTRACE(Stack),
      ?log(critical, "Build aborted: " ++ Fmt, Args),
      ?log(debug, "Panic stacktrace: ~p", [Stack]),
      erlang:halt(1);
    EC:Err ?BIND_STACKTRACE(Stack) ->
      ?GET_STACKTRACE(Stack),
      ?log( critical
          , "Uncaught ~p in ~p: ~p~nStacktrace: ~p"
          , [EC, ?MODULE, Err, Stack]
          ),
      erlang:halt(1)
  end.

-spec panic(string(), term()) -> no_return().
panic(Format, Args) ->
  exit({panic, Format, Args}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec global_config_model() -> lee:module().
global_config_model() ->
  #{ cache_dir =>
       {[value, mustache, os_env],
        #{ oneliner => "Directory where global caches are located"
         , type => string()
         , default => filename:basedir(user_cache, "tendon")
         , os_env => "TENDON_CACHE_DIR"
         , doc_remark => "Default value is platform-dependent."
         }}
   , parallel_tasks =>
       {[value, cli_param],
        #{ oneliner => "Limit the number of parallel jobs"
         , type => non_neg_integer()
         , default => 0
         , cli_short => "j"
         , doc_remark => "0 denotes unlimited"
         }}
   , keep_going =>
       {[value, cli_param],
        #{ oneliner => "Keep scheduling new tasks after failure is detected"
         , type => boolean()
         , default => false
         , cli_operand => "keep-going"
         , cli_short => "K"
         }}
   , always_make =>
       {[value, cli_param],
        #{ oneliner => "Unconditionally make all targets"
         , type => boolean()
         , default => false
         , cli_operand => "always-make"
         , cli_short => "B"
         }}
   , show_top =>
       {[value, cli_param],
        #{ oneliner => "Show statistics about task time"
         , type => boolean()
         , default => false
         , cli_operand => "top"
         }}
   , show_depgraph =>
       {[value, cli_param],
        #{ oneliner => "Generate dependency graph in dot format"
         , type => boolean()
         , default => false
         , cli_operand => "depgraph"
         }}
   , verbosity =>
       {[value, cli_param],
       #{ onliner => "Verbosity of console output"
        , type => tendon_event:level()
        , default => error
        , cli_short => "v"
        , cli_operand => "verbosity"
        }}
   , get_version =>
       {[value, cli_param],
        #{ oneliner => "Print version and exit"
         , type => boolean()
         , default => false
         , cli_operand => "version"
         }}
   , get_help =>
       {[value, cli_param],
        #{ oneliner => "Get help about a command and exit"
         , type => atom()
         , default => undefined
         , cli_operand => "help"
         , cli_short => "h"
         }}
   , profile =>
       {[value, cli_param],
        #{ oneliner => "Build profile"
         , type => profile()
         , default => default
         , cli_operand => "profile"
         , cli_short => "p"
         }}
   }.

start_logging() ->
  application:set_env(hut, level, ?cfg([verbosity])),
  %% TODO
  Handlers = [],
  {ok, _} = tendon_event:start_link([tendon_evt_console | Handlers]),
  ok.

-spec tendon_main([string()]) -> ok | error.
tendon_main(Opts) ->
  ?log(debug, "Configuration dump: ~p", [lee_server:dump()]),
  Seeds = [tendon_plugin:seed(P) || P <- tendon_plugin:plugins()],
  Seed = tendon_lib:merge_digraphs(Seeds),
  ?log(debug, "Seed: ~p", [Seed]),
  ensure_work_dirs(),
  Resources = case ?cfg([parallel_tasks]) of
                0 -> #{};
                N -> #{jobs => N}
              end,
  TGOpts = #{ keep_going     => ?cfg([keep_going])
            , disable_guards => ?cfg([always_make])
            , resources      => Resources
            , event_manager  => tendon_event
            },
  ?log(debug, "task_graph options: ~p", [TGOpts]),
  case task_graph:run_graph(tendon_graph, TGOpts, Seed) of
    {ok, Result} ->
      ?log(notice, "Build succeeded.", []),
      ok;
    {error, Term} ->
      ?log(critical, "Build failed: ~p", [Term]),
      error %;
    %% {aborted, Term} ->
    %%   ?log(critical, "Build aborted: ~p", Term),
    %%   error
  end.

-spec read_global_config([string()]) -> ok.
read_global_config(Opts) ->
  Transaction =
    fun(Model, _) ->
        GlobalCfg = lee_os_env:read(Model) ++ lee_cli:read(Model, Opts),
        {ok, GlobalCfg}
    end,
  change_config(Transaction).

-spec read_project_config(package_id(), filelib:dirname()) -> ok.
read_project_config(Package, ProjectDir) ->
  Transaction =
    fun(Model, _) ->
        MaybeReadCfgFile =
          fun(File, Acc0 = {_, Patch0}) ->
              FullPath = filename:join(ProjectDir, File),
              case filelib:is_file(FullPath) of
                true ->
                  Patch = lee_consult:read(Model, FullPath, [project_config]),
                  {false, Patch ++ Patch0};
                false ->
                  Acc0
              end
          end,
        {Empty, Cfg0} = lists:foldl( MaybeReadCfgFile
                                   , {true, []}
                                   , ["rebar.config", "tendon.config"]
                                   ),
        %% TODO: This is hacky!!!! lee_consult should be smarter
        Cfg = lists:map( fun({set, [project, ?children | RestKey], Val}) ->
                             {set, [project, ?lcl([Package])] ++ RestKey, Val}
                         end
                       , Cfg0
                       ),
        Empty andalso throw(ProjectDir ++ " is not a valid tendon project directory"),
        {ok, Cfg}
    end,
  change_config(Transaction).

-spec change_config(fun()) -> ok.
change_config(Transaction) ->
  case lee_server:patch(Transaction) of
    ok ->
      ok;
    {error, Err0} ->
      ?log(debug, "Failed to patch config: ~p", [Err0]),
      case Err0 of
        {invalid_config, LeeErrs, LeeWarns} ->
          Err = lee_lib:format( "Errors:~n~s~nWarnings:~n~s"
                              , [ string:join(LeeErrs, "\n")
                                , string:join(LeeWarns, "\n")
                                ]
                              );
        {throw, {error, Err}} -> ok;
        {throw, Err}          -> ok;
        {error, Err}          -> ok;
        Err                   -> ok
      end,
      ?log(critical, "Invalid configuration!~n~s", [lee_lib:term_to_string(Err)]),
      erlang:halt(1)
  end.

-spec maybe_show_help_and_exit() -> ok.
maybe_show_help_and_exit() ->
  case ?cfg([get_help]) of
    undefined ->
      ok;
    _ ->
      io:format("TODO: not implemented"),
      erlang:halt(0)
  end.

-spec patch_project_model(tendon_plugin:plugin(), lee:module()) ->
                             lee:module().
patch_project_model(Plugin, Module0) ->
  Module1 =
    lee_model:map_vals( fun(Node = {_, #{undocumented := true}}) ->
                            Node;
                           ({MT, MV}) ->
                            {[consult, project_config | MT], MV};
                           ({MT, MV, Children}) ->
                            {[consult, project_config | MT], MV, Children}
                        end
                      , Module0),
  lee:namespace([Plugin], Module1).

-spec merged_project_model() -> lee:module().
merged_project_model() ->
  ProjectModels = [patch_project_model(P, P:project_model())
                   || P <- tendon_plugin:plugins()],
  %% HACK: We know that project namespaces don't collide, hence
  %% regular map merge is fine:
  lists:foldl(fun maps:merge/2, #{}, ProjectModels).

-spec ensure_work_dirs() -> ok.
ensure_work_dirs() ->
  WorkDir = ?cfg_dir([project, ?lcl([root]), tendon_core, base_dir]),
  CacheDir = ?cfg_dir([cache_dir]),
  Dirs = [ filename:join(WorkDir, "bin")
         , filename:join(WorkDir, "lib")
         ],
  lists:foreach(fun tendon_lib:ensure_dir/1, Dirs),
  ok.
