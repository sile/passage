%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%%
-module(passage_sampler).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([new/2]).
-export([is_sampler/1]).
-export([get_module/1]).
-export([get_state/1]).

-export_type([sampler/0]).
-export_tyep([state/0]).

%%------------------------------------------------------------------------------
%% Application Internal API
%%------------------------------------------------------------------------------
-export([is_sampled/3]).

%%------------------------------------------------------------------------------
%% Callback API
%%------------------------------------------------------------------------------
-callback is_sampled(state(), passage:operation_name(), passage:tags()) ->
    boolean().

%%------------------------------------------------------------------------------
%% Macros and Records
%%------------------------------------------------------------------------------
-define(SAMPLER, ?MODULE).

-record(?SAMPLER,
        {
          module :: module(),
          state  :: state()
        }).

%%------------------------------------------------------------------------------
%% Exported Types
%%------------------------------------------------------------------------------
-opaque sampler() :: #?SAMPLER{}.

-type state() :: term().

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
-spec new(module(), state()) -> sampler().
new(Module, State) ->
    #?SAMPLER{module = Module, state = State}.

-spec is_sampler(sampler() | term()) -> boolean().
is_sampler(X) ->
    is_record(X, ?SAMPLER).

-spec get_module(sampler()) -> module().
get_module(Sampler) ->
    Sampler#?SAMPLER.module.

-spec get_state(sampler()) -> state().
get_state(Sampler) ->
    Sampler#?SAMPLER.state.

%%------------------------------------------------------------------------------
%% Application Internal Functions
%%------------------------------------------------------------------------------
-spec is_sampled(sampler(), passage:operation_name(), passage:tags()) -> boolean().
is_sampled(#?SAMPLER{module = Module, state = State}, Name, Tags) ->
    Module:is_sampled(State, Name, Tags).
