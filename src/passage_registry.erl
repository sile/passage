-module(passage_registry).

-behavior(gen_server).

-export([start_link/0]).

-export([get_sampler/1]).
-export([get_tracer_module/1]). % TODO: get_tracer(?)
-export([register_tracer/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(STATE, ?MODULE).

-record(?STATE,
        {
          table :: ets:tab()
        }).

-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_sampler(passage:tracer()) -> passage_sampler:sampler().
get_sampler(Tracer) ->
    try ets:lookup(?MODULE, {sampler, Tracer}) of
        [{_, Sampler}] -> Sampler;
        _              -> passage_sampler_null:new()
    catch
        error:badarg -> passage_sampler_null:new()
    end.

-spec get_tracer_module(passage:tracer()) -> module().
get_tracer_module(Tracer) ->
    try ets:lookup(?MODULE, {tracer, Tracer}) of
        [{_, Tracer}] -> Tracer;
        _             -> passage_tracer_null % TODO
    catch
        error:badarg -> passage_tracer_null
    end.

-spec register_tracer(passage:tracer_id(), module(), passage_sampler:sampler(), [passage_reporter:reporter()]) ->
                             ok | {error, Reason :: term()}.
register_tracer(TracerId, Module, Sampler, Reporters) ->
    error(unimplemented, [TracerId, Module, Sampler, Reporters]).

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
