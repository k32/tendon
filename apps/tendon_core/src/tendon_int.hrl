-ifndef(TENDON_INT_HRL).
-define(TENDON_INT_HRL, 1).

-include("tendon.hrl").

-ifndef(OLD_TIME_UNITS).
-define(tg_timeUnit, millisecond).
-else.
-define(tg_timeUnit, milli_seconds).
-endif.

-ifdef(OTP_RELEASE).
-define(BIND_STACKTRACE(V), : V).
-define(GET_STACKTRACE(V), ok).
-else.
-define(BIND_STACKTRACE(V),).
-define(GET_STACKTRACE(V), V = erlang:get_stacktrace()).
-endif.

-endif.
