-module(passage_registry).

-behavior(gen_server).

-export([start_link/0]).

-export([get_sampler/1]).
-export([get_tracer_module/1]). % TODO: get_tracer(?)
-export([get_reporter/1]).
-export([register_tracer/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(STATE, ?MODULE).
-define(TABLE, ?MODULE).
-record(?STATE,
        {
          tracers = [] :: [passage:tracer_id()]
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

-spec get_reporter(passage:tracer_id()) -> passage_reporter:reporter().
get_reporter(TracerId) ->
    try ets:lookup(?MODULE, {reporter, TracerId}) of
        [{_, Reporter}] -> Reporter;
        _               -> passage_reporter_null:new()
    catch
        error:badarg -> passage_reporter_null:new()
    end.

-spec get_tracer_module(passage:tracer()) -> module().
get_tracer_module(Tracer) ->
    try ets:lookup(?MODULE, {module, Tracer}) of
        [{_, Tracer}] -> Tracer;
        _             -> passage_tracer_null % TODO
    catch
        error:badarg -> passage_tracer_null
    end.

-spec register_tracer(passage:tracer_id(), module(), passage_sampler:sampler(),
                      passage_reporter:reporter()) ->
                             ok | {error, Reason :: term()}.
register_tracer(TracerId, Module, Sampler, Reporter) ->
    gen_server:call(?MODULE, {register, {TracerId, Module, Sampler, Reporter}}).

init([]) ->
    ets:new(?MODULE, [named_table, protected, {read_concurrency, true}]),
    State = #?STATE{},
    {ok, State}.

handle_call({register, Args}, _From, State) ->
    handle_register(Args, State);
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

-spec handle_register(Args, #?STATE{}) -> {reply, Result, #?STATE{}} when
      Args :: {passage:tracer_id(), module(), passage_sampler:sampler(),
               passage_reporter:reporter()},
      Result :: ok | {error, already_registered | term()}.
handle_register({TracerId, Module, Sampler, Reporter}, State0) ->
    ets:insert(?TABLE, {{sampler, TracerId}, Sampler}),
    ets:insert(?TABLE, {{module, TracerId}, Module}),
    ets:insert(?TABLE, {{reporter, TracerId}, Reporter}),
    State1 = State0#?STATE{tracers = [TracerId | State0#?STATE.tracers]},
    {reply, ok, State1}.
