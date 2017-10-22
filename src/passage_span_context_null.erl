%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A null implementation of the {@link passage_span_context} behaviour.
%%
%% Note that this implementation is provided only for testing purposes.
%%
%% === Examples ===
%%
%% ```
%% Context = passage_span_context_null,
%% Sampler = passage_sampler_null:new(),
%% Reporter = passage_reporter_null:new(),
%%
%% ok = passage_tracer_registry:register(foo, Context, Sampler, Reporter),
%% [foo] = passage_tracer_registry:which_tracers(),
%% '''
-module(passage_span_context_null).

-behaviour(passage_span_context).

%%------------------------------------------------------------------------------
%% 'passage_span_context' Callback API
%%------------------------------------------------------------------------------
-export([make_span_context_state/1, inject_span_context/4, extract_span_context/3]).

%%------------------------------------------------------------------------------
%% 'passage_span_context' Callback Functions
%%------------------------------------------------------------------------------
%% @private
make_span_context_state(_Refs) ->
    undefined.

%% @private
inject_span_context(_Context, _Format, _InjectFun, Carrier) ->
    Carrier.

%% @private
extract_span_context(_Format, _IterateFun, _Carrier) ->
    error.
