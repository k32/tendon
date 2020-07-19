-module(anvl_make).

-behaviour(gen_server).

%% API
-export([start_link/1, want/2, provide/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-include_lib("hut/include/hut.hrl").

-define(SERVER, ?MODULE).

-define(DONE_TAB, anvl_done_tab).

-type tag() :: atom().

-type target() :: {tag(), _Args}.

-type provider() :: {tag(), fun((_) -> _)}.

-type from() :: {pid(), _Tag}.

-record(promise,
        { worker       :: pid() | undefined
        , waiting = [] :: list()
        }).

-type promises() :: #{target() => #promise{}}.

-type providers() :: #{tag() => fun((_) -> _)}.

-record(s,
        { providers      :: providers()
        , promises = #{} :: promises()
        , workers  = #{} :: #{pid() => boolean()}
        }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Block execution of the process until a dependency is
%% satisfied, return value of the dependency
-spec want(tag(), _Args) -> term().
want(Tag, Args) ->
  Target = {Tag, Args},
  case ets:lookup(?DONE_TAB, Target) of
    [Result] ->
      Result;
    [] ->
      gen_server:call(?SERVER, {want, Target}, infinity)
  end.

%% @doc Satisfy dependencies
-spec provide([{target(), term()}]) -> ok.
provide(Targets) ->
  gen_server:call(?SERVER, {provide, Targets}).

%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link([provider()]) -> {ok, Pid :: pid()} |
        {error, Error :: {already_started, pid()}} |
        {error, Error :: term()} |
        ignore.
start_link(Providers) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Providers, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Providers0) ->
  process_flag(trap_exit, true),
  Providers = maps:from_list(Providers0),
  {ok, #s{providers = Providers}}.

handle_call({want, Target}, From, State) ->
  do_want(Target, From, State);
handle_call({provide, Targets}, _From, State0) ->
  State = lists:fold(fun resolve_target/2, State0, Targets),
  {reply, ok, State};
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

handle_cast({complete, Pid}, State0 = #s{workers = Workers}) ->
  State = State0#s{workers = maps:remove(Pid, Workers)},
  check_progress(State),
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
  #s{workers = Workers} = State,
  case maps:is_key(Pid, Workers) of
    true ->
      handle_failure(Pid, Reason, State);
    false ->
      {stop, Reason}
  end;
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec do_want(target(), from(), #s{}) -> {noreply, #s{}}
                                       | {reply, _, #s{}}.
do_want(Target, From = {Pid, _}, State0) ->
  case ets:lookup(?DONE_TAB, Target) of
    [Result] ->
      %% Handle a race condition:
      {reply, Result, State0};
    [] ->
      #s{ providers = Providers
        , workers   = Workers0
        , promises  = Promises0
        } = State0,
      %% Try to find or spawn a worker for `Target':
      Promise = #promise{worker = Worker} =
        maybe_spawn_worker(Target, From, Promises0, Providers),
      Promises = Promises0 #{target => Promise},
      %% Add new worker to the active worker set:
      Workers1 = if is_pid(Worker) ->
                     maps:merge(#{Worker => false}, Workers0);
                    true ->
                     Workers0
                 end,
      Workers = case Workers1 of
                  #{Pid := _} -> Workers1 #{Pid => true};
                  _           -> Workers1
                end,
      State = State0#s{ workers  = Workers
                      , promises = Promises
                      },
      check_progress(State),
      {noreply, State}
  end.

-spec do_provide([{target(), term()}], #s{}) -> #s{}.
do_provide(Targets, State0) ->

-spec maybe_spawn_worker(target(), from(), promises(), providers()) ->
        #promise{}.
maybe_spawn_worker(Target, From, Promises, Providers) ->
  {Tag, Args} = Target,
  case Promises of
    #{Target := Promise0 = #promise{waiting = Waiting}} ->
      Promise0#promise{waiting = [From|Waiting]};
    _ ->
      Worker = case Providers of
                 #{Tag := Fun} -> spawn_worker(Fun, Tag, Args);
                 _             -> undefined
               end,
      #promise{ worker = Worker
              , waiting = From
              }
  end.

-spec spawn_worker(fun((_) -> _), tag(), _Args) -> pid().
spawn_worker(Fun, Tag, Args) ->
  spawn_link(fun() ->
                 ?set_process_metadata(#{ domain => [anvl, target, Tag]
                                        , target => {Tag, Args}
                                        }),
                 Ret = Fun(Args),
                 provide([{{Tag, Args}, Ret}]),
                 complete(self())
             end).

-spec complete(pid()) -> ok.
complete(Pid) ->
  gen_server:cast(?SERVER, {complete, Pid}).

-spec handle_failure(pid(), _Reason, #s{}) -> {stop, _}.
handle_failure(Pid, Reason, State) ->
  %% TODO:
  error({target_failed, Reason}).

-spec check_progress(#s{}) -> ok.
check_progress(#s{workers = Workers, promises = Promises}) ->
  case maps:size(Promises) of
    0 ->
      ok;
    _ ->
      case maps:fold(fun(_, Val, Acc) -> Val and Acc end, true, Workers) of
        true ->
          Deps = maps:keys(Promises),
          error({unsatisfied_dependencies, Deps});
        _ ->
          ok
      end
  end.
