%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Process Reporter.
%%
%% This reporter will send the finished spans to a process.
%%
%% === Examples ===
%%
%% ```
%% %% Registers `tracer'
%% Context = passage_span_context_null,
%% Sampler = passage_sampler_all:new(),
%% Reporter = passage_reporter_process:new(self(), span),
%% ok = passage_tracer_registry:register(tracer, Context, Sampler, Reporter),
%%
%% %% Starts and finishes a span
%% Span = passage:start_span(example, tracer),
%% passage:finish_span(Span),
%%
%% %% Receives the finish span
%% receive
%%     {span, Span} ->
%%         example = passage_span:get_operation_name(Span)
%% end.
%% '''
-module(passage_reporter_process).

-behaviour(passage_reporter).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([new/2]).

%%------------------------------------------------------------------------------
%% 'passage_reporter' Callback API
%%------------------------------------------------------------------------------
-export([report/2]).

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
%% @doc Makes a new reporter.
%%
%% This reporter will send the finished spans to `DestinationPid'
%% by the message `{Tag, Span}'.
-spec new(pid(), term()) -> passage_reporter:reporter().
new(DestinationPid, Tag) ->
    passage_reporter:new(?MODULE, {DestinationPid, Tag}).

%%------------------------------------------------------------------------------
%% 'passage_reporter' Callback Functions
%%------------------------------------------------------------------------------
%% @private
report({DestinationPid, Tag}, Span) ->
    DestinationPid ! {Tag, Span}.
