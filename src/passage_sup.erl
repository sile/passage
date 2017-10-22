-module(passage_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Registry = #{
      id      => passage_tracer_registry,
      start   => {passage_tracer_registry, start_link, []},
      restart => permanent
     },
    {ok, {#{}, [Registry]} }.
