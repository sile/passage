%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Null Reporter.
%%
%% This reporter discards all the spans.
%%
%% === Examples ===
%%
%% ```
%% Context = passage_span_context_null,
%% Sampler = passage_sampler_null:new(),
%% Reporter = passage_reporter_null:new(),
%%
%% ok = passage_tracer_registry:register(foo, Context, Sampler, Reporter).
%% '''
-module(passage_reporter_null).

-behaviour(passage_reporter).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([new/0]).

%%------------------------------------------------------------------------------
%% 'passage_reporter' Callback API
%%------------------------------------------------------------------------------
-export([report/2]).

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
%% @doc Makes a new reporter.
-spec new() -> passage_reporter:reporter().
new() ->
    passage_reporter:new(?MODULE, undefined).

%%------------------------------------------------------------------------------
%% 'passage_reporter' Callback Functions
%%------------------------------------------------------------------------------
%% @private
report(_State, _Span) ->
    ok.
