-ifndef(TENDON_HRL).
-define(TENDON_HRL, 1).

-include_lib("hut/include/hut.hrl").
-include_lib("lee/include/lee.hrl").
-include_lib("task_graph/include/task_graph.hrl").
-include_lib("typerefl/include/types.hrl").

%% Record for structured build flow events
-record(td_event,
        { timestamp  :: integer()
        , provider   :: tendon_plugin:provider()
        , message    :: string()
        , level      :: tendon_event:level()
        , data       :: #{atom() => term()}
        , raw_output :: iolist()
        }).

-record(td_task,
        { provider   :: module()
        , data       :: term()
        }).

-define(EVENT(Level, Message),
        tendon_event:event(?PROVIDER, Level, Message)).
-define(EVENT(Level, Message, Data),
        tendon_event:event(?PROVIDER, Level, Message, Data)).
-define(EVENT(Level, Message, Data, RawOutput),
        tendon_event:event(?PROVIDER, Level, Message, Data, RawOutput)).

-define(cfg(A), lee_server:get_d(A)).

-define(cfg_template(A), tendon_lib:render_template(A)).

-define(cfg_dir(A), tendon_lib:render_dir(A)).
-define(cfg_dirs(A), tendon_lib:render_dirs(A)).

-define(mk_task(Id, Data, Resources),
        tendon_provider:make_task(?MODULE, Id, Data, Resources)).

-define(mk_task(Id, Data),
        mk_task(Id, Data, [])).

-endif.
