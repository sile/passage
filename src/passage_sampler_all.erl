%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc All Sampler.
%%
%% This sampler samples all the spans.
%%
%% === Examples ===
%%
%% ```
%% Context = passage_span_context_null,
%% Sampler = passage_sampler_all:new(),
%% Reporter = passage_reporter_null:new(),
%%
%% ok = passage_tracer_registry:register(foo, Context, Sampler, Reporter).
%% '''
-module(passage_sampler_all).

-behaviour(passage_sampler).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([new/0]).

%%------------------------------------------------------------------------------
%% 'passage_sampler' Callback API
%%------------------------------------------------------------------------------
-export([is_sampled/3]).

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
%% @doc Makes a new sampler.
-spec new() -> passage_sampler:sampler().
new() ->
    passage_sampler:new(?MODULE, undefined).

%%------------------------------------------------------------------------------
%% 'passage_sampler' Callback Functions
%%------------------------------------------------------------------------------
%% @private
is_sampled(_State, _OperationName, _Tags) ->
    true.
