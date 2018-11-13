-module(tendon_lib_tests).

-include_lib("typerefl/include/types.hrl").
-include_lib("eunit/include/eunit.hrl").

test_model() ->
  Model0 = #{ my_string =>
                {[value],
                 #{ type => string()
                  , default => "my_string"
                  }}
            , my_atom =>
                {[value],
                 #{ type => atom()
                  , default => my_atom
                  }}
            , my_term =>
                {[value],
                 #{ type => term()
                  , default => {foo, bar, []}
                  }}
            , ok_recursion =>
                {[value],
                 #{ type => string()
                  , default => "{{cfg: [my_string]}}"
                  }}
            %% This passes as a fixpoint:
            , cheesy_recursion =>
                {[value],
                 #{ type => string()
                  , default => "{{cfg: [cheezy_recursion]}}"
                  }}
            , inf_recursion =>
                {[value],
                 #{ type => string()
                  , default => "foo {{cfg: [inf_recursion]}}"
                  }}
            },
  {ok, Model} = lee_model:compile( [lee:base_metamodel()]
                                 , [Model0]
                                 ),
  Model.

test_data1() ->
  lee_storage:new(lee_map_storage).

render_template_test() ->
  Model = test_model(),
  Data = test_data1(),
  Render = fun(Template) ->
                 tendon_lib:render_template(Model, Data, Template)
           end,
  %% Valid configuration getter scenarios:
  ?assertMatch({ok, "fooo"}, Render("fooo")),
  ?assertMatch({ok, "foo my_atom bar"}, Render("foo {{cfg: [my_atom]}} bar")),
  ?assertMatch({ok, "foo my_string bar"}, Render("foo {{cfg: [my_string]}} bar")),
  ?assertMatch({ok, "foo my_string bar"}, Render("foo {{cfg: [ok_recursion]}} bar")),
  ?assertMatch({ok, "{foo,bar,[]}"}, Render("{{cfg: [my_term]}}")),
  %% Error scenarios:
  ?assertMatch( {error, "Unknown template variable scope in template" ++ _}
              , Render("{{foo}}")
              ),
  ?assertMatch( {error, "Invalid configuration key:" ++ _}
              , Render("{{cfg: foo}}")
              ),
  ?assertMatch( {error, "Invalid configuration key:" ++ _}
              , Render("{{cfg: [<<'foo'>>]}}")
              ),
  ?assertMatch( {error, "Invalid configuration key:" ++ _}
              , Render("{{cfg: [foo]}}")
              ),
  ?assertMatch( {error, "Too many substitution levels in template" ++ _}
              , Render("{{cfg: [inf_recursion]}}")
              ).
