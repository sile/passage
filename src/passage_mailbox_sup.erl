-module(passage_mailbox_sup).

-behaviour(supervisor).

-export([start_child/1]).

-export([start_link/0]).

-export([init/1]).

-spec start_child(passage:tracer()) -> {ok, pid()} | {error, Reason :: term()}.
start_child(Tracer) ->
    Child = make_child_spec(Tracer),
    supervisor:start_child(?MODULE, Child).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Default = make_child_spec(passage_tracer:default_tracer()),
    {ok, {#{}, [Default]}}.

-spec make_child_spec(passage:tracer_id()) -> supervisor:child_spec().
make_child_spec(Tracer) ->
    #{
       id      => Tracer,
       start   => {passage_mailbox, start_link, [Tracer]},
       restart => permanent
     }.
