-module(passage_registry).

-behavior(gen_server).

-export([start_link/0]).

-export([get_default_tracer/0]).
-export([get_sampler/1]).
-export([get_tracer_module/1]). % TODO: get_tracer(?)

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(STATE, ?MODULE).

-record(?STATE,
        {
          table :: ets:tab()
        }).

-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_default_tracer() -> passage:tracer().
get_default_tracer() ->
    error(unimplemented).

-spec get_sampler(passage:tracer()) -> passage_sampler:sampler().
get_sampler(Tracer) ->
    error(unimplemented, [Tracer]).

-spec get_tracer_module(passage:tracer()) -> module().
get_tracer_module(Tracer) ->
    error(unimplemented, [Tracer]).

init([]) ->
    Table = ets:new(?MODULE, [named_table, protected, {read_concurrency, true}]),
    State = #?STATE{table = Table},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
