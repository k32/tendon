-module(tendon_compile).

-include_lib("tendon_core/include/tendon.hrl").

-behavior(tendon_plugin).

-export([model/0, project_model/0, providers/0]).

model() ->
  Model =
    #{ action =>
         {[map, cli_action],
          #{ cli_operand => "compile"
           , ?key_elements => [[apps]]
           },
          #{ apps =>
               {[value, cli_positional],
                #{ oneliner => "List of apps that should be compiled"
                 , type     => list(tendon_core:app_id())
                 , cli_arg_position => rest
                 }}
           }}
     },
  lee:namespace([?MODULE], Model).

project_model() ->
  #{ erl_opts =>
       {[value],
        #{ oneliner => "Options passed to erlc"
         , type     => list()
         , default  => []
         , file_key => erl_opts
         }}
   }.

providers() ->
  [].
