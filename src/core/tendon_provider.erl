-module(tendon_provider).

%% Behavior describing top-level

-include("tendon_int.hrl").

-export_type([ verify_option/0
             ]).

-export([ name/1
        , options/1
        , load_providers/1

        , is_boolean/1
        , is_pos_integer/1
        , is_string/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

%% A callback that checks type and value of an option
-type verify_option() :: fun((term()) -> ok | {error, term()}).

%%%===================================================================
%%% Callback definitions
%%%===================================================================

%% Return human-friendly name of the provider (as shown in the logs
%% and the CLI):
-callback name() -> string().

%% Return a set of configuration options introduced by this provider.
%% Note: it's fine to reuse options introduced by the upstram
%% providers, but they should not be listed here
-callback options() -> #{atom() => verify_option()}.

%% _Very short_ (<20 chars) one-liner description of what this
%% provider provides
-callback description() -> string().

%% docopt description of CLI options.
-callback cli_option() -> string().

%%%===================================================================
%%% Some standard type checks
%%%===================================================================
-spec is_boolean(term()) -> ok | {error, string()}.
is_boolean(V) when is_boolean(V) ->
  ok;
is_boolean(_) ->
  {error, "Boolean expected"}.

-spec is_pos_integer(term()) -> ok | {error, string()}.
is_pos_integer(V) when is_boolean(V), V > 0 ->
  ok;
is_pos_integer(_) ->
  {error, "Integer expected"}.

-spec is_string(term()) -> ok | {error, string()}.
is_string(S) ->
  case io_lib:printable_list(S) of
    true ->
      ok;
    false ->
      {error, "Printable string expected"}
  end.

%%%===================================================================
%%% tendon-internal functions
%%%===================================================================
name(Provider) ->
    Provider:name().

options(Provider) ->
    Provider:options().

-spec load_providers([provider()]) ->
                        #{atom() => tendon_provider:verify_option()}.
load_providers(Providers) ->
  {_, Merged} =
    lists:foldl( fun(Provider, {Size, Acc}) ->
                     case code:ensure_loaded(Provider) of
                       {ok, _} ->
                         ok;
                       {error, _} ->
                         ?CRITICAL("Bad provider", #{module => Provider})
                     end,
                     try
                       Options = tendon_provider:options(Provider),
                       Merged = maps:merge(Acc, Options),
                       MergedSize = maps:size(Merged),
                       MergedSize =:= Size + maps:size(Options)
                         orelse throw(colliding_options),
                       {MergedSize, Merged}
                     catch
                       _:Err ->
                         ?INTERNAL_ERROR( "Bad provider options"
                                        , #{ module => Provider
                                           , error => Error
                                           }
                                        )
                     end
                 end
               , {0, #{}}
               , Providers
               ).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
