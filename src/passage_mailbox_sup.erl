-module(passage_mailbox_sup).

-behaviour(supervisor).

-export([start_child/1]).

-export([start_link/0]).

-export([init/1]).

-spec start_child(passage:tracer()) -> {ok, pid()} | {error, Reason :: term()}.
start_child(Tracer) ->
    Child = #{
      id      => Tracer,
      start   => {passage_mailbox, start_link, [Tracer]},
      restart => permanent
     },
    supervisor:start_child(?MODULE, Child).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {#{}, []}}.
