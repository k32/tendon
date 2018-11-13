-module(tendon_lib).

-include("tendon_int.hrl").

-export([ render_template/3
        , render_template/1
        , render_dir/1
        , render_dirs/1

        , ensure_dir/1

        , try_get_cfg/3
        , merge_digraphs/1

        , metamodel/0
        ]).

-define(recursion_depth, 5).

-spec render_template(lee:model(), lee:data(), string() | binary()) ->
                         {ok, string()} | {error, string()}.
render_template(Model, Data, Template) when is_binary(Template) ->
  %% DO NOT escape anything
  EscapeFun = fun(A) -> A end,
  %% Transform comma-separated string to a Lee key and get it from `Data'
  GetterFun = fun(Str0) ->
                  case Str0 of
                    "cfg:" ++ Str ->
                      case typerefl:from_string(list(), Str) of
                        {ok, Key} when is_list(Key) ->
                          {ok, Term} = try_get_cfg(Model, Data, Key),
                          {ok, lee_lib:term_to_string(Term)};
                        _ ->
                          throw("Invalid configuration key: " ++ Str)
                      end;
                    _ ->
                      throw("Unknown template variable scope in template " ++ Str0)
                  end
              end,
  Options = [ {key_type, string}
            , {escape_fun, EscapeFun}
            ],
  RenderFun =
    fun
      F(_, 0) ->
        Err = lee_lib:format( "Too many substitution levels in template ~s"
                            , [Template]
                            ),
        throw(Err);
      F(Str0, N) ->
        case bbmustache:render(Str0, GetterFun) of
          Str0 -> %% Fixpoint reached
            binary_to_list(Str0);
          Str1 ->
            F(Str1, N - 1)
        end
    end,
  try
    {ok, RenderFun(Template, ?recursion_depth)}
  catch
    Err = {error, _} -> Err;
    Err -> {error, Err}
  end;
render_template(Model, Data, Template) ->
  case io_lib:char_list(Template) of
    true ->
      render_template(Model, Data, list_to_binary(Template));
    false ->
      {error, "Wrong type of argument"}
  end.

-spec validate_template( lee:model()
                       , lee:data()
                       , lee:key()
                       , string()
                       ) -> {[string()], [string()]}.
validate_template(Model, Data, Key, _) ->
  try lee:get(Model, Data, Key) of
    Value ->
      case render_template(Model, Data, Value) of
        {ok, _} ->
          {[], []};
        {error, Str} ->
          Msg = lee_lib:format( "Key: ~p~n"
                                "Invalid template: ~p~n"
                                "Error: ~s~n"
                              , [Key, Value, Str]
                              ),
          {[Msg], []}
      end
  catch
    _:_ -> %% Not our problem
      {[], []}
  end.

metamodel() ->
  #{ metatype =>
       #{ mustache =>
            {[metatype],
             #{ validate_node => fun validate_template/4
              }}
        }
   }.

-spec render_template(lee:key()) -> string().
render_template(Key) ->
  {ok, Ret} =
    lee_server:run_t(
      fun(Model, Data) ->
          Template = lee_server:get(Key),
          %% Should always return {ok, _} since the temolate is
          %% validated:
          render_template(Model, Data, Template)
      end),
  Ret.

-spec render_dir(lee:key()) -> file:filename().
render_dir(Key) ->
  extend_home(render_template(Key)).

-spec render_dirs(lee:key()) -> [file:filename()].
render_dirs(Key) ->
  filelib:wildcard(render_dir(Key)).

-spec extend_home(string()) -> file:filename().
extend_home(Str0) ->
  %% TODO FIXME: Windows is probably broken
  case Str0 of
    [$~] ->
      os:getenv("HOME");
    [$~, $/|Str01] ->
      Home = os:getenv("HOME"),
      filename:join(Home, Str01);
    _ ->
      Str0
  end.

-spec ensure_dir(file:filename()) -> ok.
ensure_dir(Dirname) ->
  filelib:ensure_dir(filename:join(Dirname, "dummy")).

-spec try_get_cfg(lee:model(), lee:data(), lee:key()) ->
                     {ok, term()}.
try_get_cfg(Model, Data, Key) ->
  MKey = lee_model:get_model_key(Key),
  try lee_model:get(MKey, Model) of
      _ -> {ok, lee:get(Model, Data, Key)}
  catch
    EC:Err ?BIND_STACKTRACE(Stack) ->
      ?GET_STACKTRACE(Stack),
      ?slog(error, #{ what => "Config read error"
                    , error_class => EC
                    , stacktrace => Stack
                    , error => Err
                    }),
      throw(lee_lib:format("Invalid configuration key: ~p", Key))
  end.

-spec merge_digraphs([tendon_core:digraph()]) ->
                        tendon_core:digraph().
merge_digraphs(GG) ->
  Fun = fun({Vtx, Edg}, {AccVtx, AccEdg}) ->
            {Vtx ++ AccVtx, Edg ++ AccEdg}
        end,
  lists:foldl(Fun, {[], []}, GG).