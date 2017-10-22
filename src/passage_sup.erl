%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @private
-module(passage_sup).

-behaviour(supervisor).

%%------------------------------------------------------------------------------
%% Application Internal API
%%------------------------------------------------------------------------------
-export([start_link/0]).

%%------------------------------------------------------------------------------
%% 'supervisor' Callback API
%%------------------------------------------------------------------------------
-export([init/1]).

%%------------------------------------------------------------------------------
%% Application Internal Functions
%%------------------------------------------------------------------------------
%% @private
-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%------------------------------------------------------------------------------
%% @private
init([]) ->
    Registry = #{
      id      => passage_tracer_registry,
      start   => {passage_tracer_registry, start_link, []},
      restart => permanent
     },
    {ok, {#{}, [Registry]} }.
