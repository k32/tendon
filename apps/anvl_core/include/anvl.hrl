-ifndef(ANVL_HRL).
-define(ANVL_HRL, 1).

-include_lib("hut/include/hut.hrl").
-include_lib("lee/include/lee.hrl").
-include_lib("task_graph/include/task_graph.hrl").
-include_lib("typerefl/include/types.hrl").

-record(td_task,
        { provider   :: module()
        , data       :: term()
        }).

-define(EVENT(Level, Message),
        anvl_event:event(?PROVIDER, Level, Message)).
-define(EVENT(Level, Message, Data),
        anvl_event:event(?PROVIDER, Level, Message, Data)).
-define(EVENT(Level, Message, Data, RawOutput),
        anvl_event:event(?PROVIDER, Level, Message, Data, RawOutput)).

-define(cfg(A), lee_server:get_d(A)).

-define(cfg_template(A), anvl_lib:render_template(A)).

-define(cfg_dir(A), anvl_lib:render_dir(A)).
-define(cfg_dirs(A), anvl_lib:render_dirs(A)).

-define(mk_task(Id, Data, Resources),
        anvl_provider:make_task(?MODULE, Id, Data, Resources)).

-define(mk_task(Id, Data),
        mk_task(Id, Data, [])).

-define(root_project, root).

-define(proj(Project), project, ?lcl(Project)).

-define(proj, ?proj(?root_project)).

-endif.
