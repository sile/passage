%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Span Sampler.
%%
%% Note that this component has not been described in the
%% <a href="https://github.com/opentracing/specification/blob/1.1/specification.md">OpenTracing Specification</a>.
%%
%% === Callbacks ===
%%
%% This module requires following callback:
%%
%% ```
%% %% @doc Determines to sample the next span which has the given name and tags.
%% -callback is_sampled(state(), passage:operation_name(), passage:tags()) -> boolean().
%% '''
-module(passage_sampler).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([new/2]).
-export([is_sampler/1]).
-export([get_module/1]).
-export([get_state/1]).
-export([is_sampled/1]).

-export_type([sampler/0]).
-export_tyep([state/0]).

%%------------------------------------------------------------------------------
%% Callback API
%%------------------------------------------------------------------------------
-callback is_sampled(state(), passage:operation_name(), passage:tags()) -> boolean().

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
%% Sampler.

-type state() :: term().
%% Implementation-dependent state.

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
%% @doc Makes a new sampler.
%%
%% Note that `Module' must be a implementation module of `passage_sampler' behaviour.
-spec new(module(), state()) -> sampler().
new(Module, State) ->
    #?SAMPLER{module = Module, state = State}.

%% @doc Returns `true' if `X' is a sampler, otherwise `false'.
-spec is_sampler(sampler() | term()) -> boolean().
is_sampler(X) ->
    is_record(X, ?SAMPLER).

%% @doc Returns the module of `Sampler'.
-spec get_module(sampler()) -> module().
get_module(Sampler) ->
    Sampler#?SAMPLER.module.

%% @doc Returns the state of `Sampler'.
-spec get_state(sampler()) -> state().
get_state(Sampler) ->
    Sampler#?SAMPLER.state.

%% @doc Determines to sample the next span which has the given name and tags.
-spec is_sampled(sampler(), passage:operation_name(), passage:tags()) -> boolean().
is_sampled(#?SAMPLER{module = Module, state = State}, Name, Tags) ->
    Module:is_sampled(State, Name, Tags).
