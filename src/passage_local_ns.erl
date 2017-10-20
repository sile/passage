-module(passage_local_ns).

-export([child_spec/0]).
-export([mailbox_name/1]).

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    local:name_server_child_spec(?MODULE).

-spec mailbox_name(jaegerl:tracer()) -> local:otp_name().
mailbox_name(Tracer) ->
    local:otp_name({?MODULE, {mailbox, Tracer}}).
