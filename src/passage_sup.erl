-module(passage_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Registry = #{
      id      => passage_registry,
      start   => {passage_registry, start_link, []},
      restart => permanent
     },
    NameServer = passage_local_ns:child_spec(),
    MailboxSup = #{
      id    => passage_mailbox_sup,
      start => {passage_mailbox_sup, start_link, []},
      type  => supervisor
     },
    {ok, {#{stragety => rest_for_one}, [Registry, NameServer, MailboxSup]} }.
