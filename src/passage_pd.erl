%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Process Dictionary version of {@link passage}.
%%
%% The functions in this module operate on the span
%% which stored in the process dictionary of the calling process.
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
%% %% Starts a root span
%% ok = passage_pd:start_root_span(example_root, tracer),
%%
%% %% Starts a child span
%% ok = passage_pd:start_span(example_child),
%%
%% %% Finishes spans
%% passage_pd:finish_span(),  % child
%% passage_pd:finish_span(),  % root
%%
%% %% Receives the finished spans
%% receive {span, FinishedChildSpan} -> ok end,
%% receive {span, FinishedRootSpan} -> ok end.
%% '''
-module(passage_pd).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([start_root_span/2, start_root_span/3]).
-export([start_span/1, start_span/2]).
-export([finish_span/0, finish_span/1]).
-export([with_root_span/3, with_root_span/4]).
-export([with_span/2, with_span/3]).
-export([current_span/0]).
-export([set_operation_name/1]).
-export([set_tags/1]).
-export([set_baggage_items/1]).
-export([get_baggage_items/0]).
-export([log/1, log/2]).

%%------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------
-define(ANCESTORS_KEY, passage_span_ancestors).

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
%% @equiv start_root_span(OperationName, Tracer, [])
-spec start_root_span(passage:operation_name(), passage:tracer_id()) -> ok.
start_root_span(OperationName, Tracer) ->
    start_root_span(OperationName, Tracer, []).

%% @doc Starts a root span.
%%
%% The started span will be pushed to the process dictionary of the calling process.
%%
%% If the sampler associated with `Tracer' does not sample the span,
%% the value of span will be `undefined'.
-spec start_root_span(passage:operation_name(), passage:tracer_id(),
                      passage:start_root_span_options()) -> ok.
start_root_span(OperationName, Tracer, Options) ->
    Span = passage:start_root_span(OperationName, Tracer, Options),
    put_ancestors([Span | get_ancestors()]).

%% @equiv start_span(OperationName, [])
-spec start_span(passage:operation_name()) -> ok.
start_span(OperationName) ->
    start_span(OperationName, []).

%% @doc Starts a span.
%%
%% The started span will be pushed to the process dictionary of the calling process.
%%
%% If there is no sampled span references, the value of span will be `undefined'.
-spec start_span(passage:operation_name(), passage:start_span_options()) -> ok.
start_span(OperationName, Options) ->
    Ancestors = get_ancestors(),
    Options1 =
        case Ancestors of
            []           -> Options;
            [Parent | _] ->
                Refs = proplists:get_value(refs, Options, []),
                [{refs, [{child_of, Parent} | Refs]} | Options]
        end,
    Span = passage_span:start(OperationName, Options1),
    put_ancestors([Span | Ancestors]).

%% @equiv finish_span([])
-spec finish_span() -> ok.
finish_span() ->
    finish_span([]).

%% @doc Pops the current span from process dictionary and finishes the span.
%%
%% The finished span will be sent an external observer via
%% the reporter associated with the tracer of the span.
-spec finish_span(passage:finish_span_options()) -> ok.
finish_span(Options) ->
    case pop_span() of
        undefined -> ok;
        Span      -> passage:finish_span(Span, Options)
    end.

%% @equiv with_root_span(OperationName, Tracer, [], Fun)
-spec with_root_span(passage:operation_name(), passage:tracer_id(), Fun) -> Result when
      Fun :: fun (() -> Result),
      Result :: term().
with_root_span(OperationName, Tracer, Fun) ->
    with_root_span(OperationName, Tracer, [], Fun).

%% @doc Starts a root span enclosing `Fun'.
-spec with_root_span(passage:operation_name(), passage:tracer_id(),
                     passage:start_span_options(), Fun) -> Result when
      Fun :: fun (() -> Result),
      Result :: term().
with_root_span(OperationName, Tracer, Options, Fun) ->
    try
        start_root_span(OperationName, Tracer, Options),
        Fun()
    after
        finish_span()
    end.

%% @equiv with_span(OperationName, [], Fun)
-spec with_span(passage:operation_name(), Fun) -> Result when
      Fun :: fun (() -> Result),
      Result :: term().
with_span(OperationName, Fun) ->
    with_span(OperationName, [], Fun).

%% @doc Starts a span enclosing `Fun'.
-spec with_span(passage:operation_name(), passage:start_span_options(), Fun) -> Result when
      Fun :: fun (() -> Result),
      Result :: term().
with_span(OperationName, Options, Fun) ->
    try
        start_span(OperationName, Options),
        Fun()
    after
        finish_span()
    end.

%% @doc Returns the current span stored in the process dictionary of the calling process.
-spec current_span() -> passage:maybe_span().
current_span() ->
    case get(?ANCESTORS_KEY) of
        undefined  -> undefined;
        []         -> undefined;
        [Span | _] -> Span
    end.

%% @doc Sets the operation name of the current span to `OperationName'.
-spec set_operation_name(passage:operation_name()) -> ok.
set_operation_name(OperationName) ->
    update_current_span(
      fun (Span) -> passage_span:set_operation_name(Span, OperationName) end).

%% @doc Sets the tags of the current span to `Tags'.
%%
%% Note that the existing tags which have different keys with `Tags' are preserved.
-spec set_tags(passage:tags()) -> ok.
set_tags(Tags) ->
    update_current_span(fun (Span) -> passage_span:set_tags(Span, Tags) end).

%% @doc Sets the baggage items of the current span to `Items'.
%%
%% Note that the existing items which have different keys with `Items' are preserved.
%%
%% See also: <a href="https://github.com/opentracing/specification/blob/1.1/specification.md#set-a-baggage-item">Set a baggage item (The OpenTracing Semantic Specification)</a>
-spec set_baggage_items(passage:baggage_items()) -> ok.
set_baggage_items(Items) ->
    update_current_span(fun (Span) -> passage_span:set_baggage_items(Span, Items) end).

%% @doc Returns the baggage items carried by the current span.
-spec get_baggage_items() -> passage:baggage_items().
get_baggage_items() ->
    Span = current_span(),
    passage:get_baggage_items(Span).

%% @equiv log(Fields, [])
-spec log(passage:log_fields()) -> ok.
log(Fields) ->
    log(Fields, []).

%% @doc Logs the `Fields' to the current span.
-spec log(passage:log_fields(), passage:log_options()) -> ok.
log(Fields, Options) ->
    update_current_span(fun (Span) -> passage:log(Span, Fields, Options) end).

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
-spec get_ancestors() -> [passage:maybe_span()].
get_ancestors() ->
    case get(?ANCESTORS_KEY) of
        undefined -> [];
        Ancestors -> Ancestors
    end.

-spec put_ancestors([passage:maybe_span()]) -> ok.
put_ancestors(Ancestors) ->
    put(?ANCESTORS_KEY, Ancestors),
    ok.

-spec update_current_span(Fun) -> ok when
      Fun :: fun ((passage_span:span()) -> passage_span:span()).
update_current_span(Fun) ->
    case get(?ANCESTORS_KEY) of
        undefined       -> ok;
        []              -> ok;
        [undefined | _] -> ok;
        [Span0 | Spans] ->
            Span1 = Fun(Span0),
            put_ancestors([Span1 | Spans])
    end.

-spec pop_span() -> passage:maybe_span().
pop_span() ->
    case get(?ANCESTORS_KEY) of
        undefined      -> undefined;
        []             -> undefined;
        [Span | Spans] ->
            put_ancestors(Spans),
            Span
    end.
