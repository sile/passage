%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%%
-module(passage_tracer_registry).

-behavior(gen_server).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([register/4]).
-export([deregister/1]).
-export([get_span_context_module/1]).
-export([get_sampler/1]).
-export([set_sampler/2]).
-export([get_reporter/1]).
-export([set_reporter/2]).
-export([which_tracers/0]).

%%------------------------------------------------------------------------------
%% Application Internal API
%%------------------------------------------------------------------------------
-export([start_link/0]).

%%------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%------------------------------------------------------------------------------
%% Macros and Records
%%------------------------------------------------------------------------------
-define(TABLE, ?MODULE).

-define(STATE, ?MODULE).

-record(?STATE,
        {
          tracers = [] :: [passage:tracer_id()]
        }).

%%------------------------------------------------------------------------------
%% Application Internal Functions
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
-spec register(Tracer, Module, Sampler, Reporter) -> ok | {error, Reason} when
      Tracer :: passage:tracer_id(),
      Module :: passage_span_context:implementation_module(),
      Sampler :: passage_sampler:sampler(),
      Reporter :: passage_reporter:reporter(),
      Reason :: already_registered | term().
register(Tracer, Module, Sampler, Reporter) ->
    Args = [Tracer, Module, Sampler, Reporter],
    is_atom(Tracer) orelse error(badarg, Args),
    passage_reporter:is_reporter(Reporter) orelse error(badarg, Args),

    gen_server:call(?MODULE, {register, {Tracer, Module, Sampler, Reporter}}).

-spec deregister(passage:tracer_id()) -> ok.
deregister(Tracer) ->
    gen_server:cast(?MODULE, {deregister, Tracer}).

-spec which_tracers() -> [passage:tracer_id()].
which_tracers() ->
    gen_server:call(?MODULE, which_tracers).

-spec get_span_context_module(passage:tracer_id()) -> {ok, Module} | error when
      Module :: passage_span_context:implementation_module().
get_span_context_module(Tracer) ->
    try ets:lookup(?TABLE, {module, Tracer}) of
        [{_, Module}] -> {ok, Module};
        _             -> error
    catch
        error:badarg -> error
    end.

-spec get_sampler(passage:tracer_id()) -> {ok, passage_sampler:sampler()} | error.
get_sampler(Tracer) ->
    try ets:lookup(?TABLE, {sampler, Tracer}) of
        [{_, Sampler}] -> {ok, Sampler};
        _              -> error
    catch
        error:badarg -> error
    end.

-spec set_sampler(passage:tracer_id(), passage_sampler:sampler()) -> ok.
set_sampler(Tracer, Sampler) ->
    gen_server:cast(?MODULE, {set_sampler, {Tracer, Sampler}}).

-spec get_reporter(passage:tracer_id()) -> {ok, passage_reporter:reporter()} | error.
get_reporter(TracerId) ->
    try ets:lookup(?TABLE, {reporter, TracerId}) of
        [{_, Reporter}] -> {ok, Reporter};
        _               -> error
    catch
        error:badarg -> error
    end.

-spec set_reporter(passage:tracer_id(), passage_reporter:reporter()) -> ok.
set_reporter(Tracer, Reporter) ->
    gen_server:cast(?MODULE, {set_reporter, {Tracer, Reporter}}).

%%------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%------------------------------------------------------------------------------
init([]) ->
    _ = ets:new(?MODULE, [named_table, protected, {read_concurrency, true}]),
    State = #?STATE{},
    {ok, State}.

handle_call({register, Args}, _From, State) ->
    handle_register(Args, State);
handle_call(which_tracers, _From, State) ->
    {ok, State#?STATE.tracers, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({deregister, Args}, State) ->
    handle_deregister(Args, State);
handle_cast({set_sampler, Args}, State)  ->
    handle_set_sampler(Args, State);
handle_cast({set_reporter, Args}, State) ->
    handle_set_reporter(Args, State);
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
-spec handle_register(Args, #?STATE{}) -> {reply, Result, #?STATE{}} when
      Args :: {passage:tracer_id(),
               passage_span_context:implementation_module(),
               passage_sampler:sampler(),
               passage_reporter:reporter()},
      Result :: ok | {error, already_registered}.
handle_register({Tracer, Module, Sampler, Reporter}, State0) ->
    case lists:member(Tracer, State0#?STATE.tracers) of
        true  -> {reply, {error, already_registered}, State0};
        false ->
            ets:insert(?TABLE, {{module, Tracer}, Module}),
            ets:insert(?TABLE, {{sampler, Tracer}, Sampler}),
            ets:insert(?TABLE, {{reporter, Tracer}, Reporter}),
            State1 = State0#?STATE{tracers = [Tracer | State0#?STATE.tracers]},
            {reply, ok, State1}
    end.

-spec handle_deregister(passage:tracer_id(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_deregister(Tracer, State0) ->
    Tracers = lists:delete(Tracer, State0#?STATE.tracers),
    ets:delete(?TABLE, {module, Tracer}),
    ets:delete(?TABLE, {sampler, Tracer}),
    ets:delete(?TABLE, {reporter, Tracer}),
    State1 = State0#?STATE{tracers = Tracers},
    {noreply, State1}.

-spec handle_set_sampler(Args, #?STATE{}) -> {noreply, #?STATE{}} when
      Args :: {passage:tracer_id(), passage_sampler:sampler()}.
handle_set_sampler({Tracer, Sampler}, State) ->
    case lists:member(Tracer, State#?STATE.tracers) of
        false -> {noreply, State};
        true  ->
            ets:insert(?TABLE, {{sampler, Tracer}, Sampler}),
            {noreply, State}
    end.

-spec handle_set_reporter(Args, #?STATE{}) -> {noreply, #?STATE{}} when
      Args :: {passage:tracer_id(), passage_reporter:reporter()}.
handle_set_reporter({Tracer, Reporter}, State) ->
    case lists:member(Tracer, State#?STATE.tracers) of
        false -> {noreply, State};
        true  ->
            ets:insert(?TABLE, {{reporter, Tracer}, Reporter}),
            {noreply, State}
    end.
