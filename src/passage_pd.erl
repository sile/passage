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
%% ok = passage_pd:start_span(example_root, [{tracer, tracer}]),
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

-include("opentracing.hrl").

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([start_span/1, start_span/2]).
-export([finish_span/0, finish_span/1]).
-export([with_span/2, with_span/3]).
-export([with_parent_span/2]).
-export([current_span/0]).
-export([set_tracer/1]).
-export([set_operation_name/1]).
-export([set_tags/1]).
-export([set_baggage_items/1]).
-export([get_baggage_items/0]).
-export([log/1, log/2]).

-export_type([with_span_option/0, with_span_options/0]).

%%------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------
-define(ANCESTORS_KEY, passage_span_ancestors).

-ifdef('OTP_RELEASE').
%% The 'OTP_RELEASE' macro introduced at OTP-21,
%% so we can use it for detecting whether the Erlang compiler supports new try/catch syntax or not.
-define(CAPTURE_STACKTRACE, :__StackTrace).
-define(GET_STACKTRACE, __StackTrace).
-else.
-define(CAPTURE_STACKTRACE, ).
-define(GET_STACKTRACE, erlang:get_stacktrace()).
-endif.

%%------------------------------------------------------------------------------
%% Exported Types
%%------------------------------------------------------------------------------
-type with_span_options() :: [with_span_option()].
%% Options for {@link with_span/3}

-type with_span_option() :: {error_if_exception, boolean()}
                          | passage:start_span_option().
%% <ul>
%%  <li>`error_if_exception': If `true', the exception which raised while executing the function will be logged and the span will be tagged as error. The default value is `true'.</li>
%% </ul>

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
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
            []              -> Options;
            [undefined | _] -> Options;
            [Parent    | _] -> [Parent | Options]
        end,
    case passage:start_span(OperationName, Options1) of
        undefined -> put_ancestors([undefined        | Ancestors]);
        Span      -> put_ancestors([{child_of, Span} | Ancestors])
    end.

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
        {_, Span} -> passage:finish_span(Span, Options)
    end.

%% @equiv with_span(OperationName, [], Fun)
-spec with_span(passage:operation_name(), Fun) -> Result when
      Fun :: fun (() -> Result),
      Result :: term().
with_span(OperationName, Fun) ->
    with_span(OperationName, [], Fun).

%% @doc Starts a span enclosing `Fun'.
-spec with_span(passage:operation_name(), with_span_options(), Fun) -> Result when
      Fun :: fun (() -> Result),
      Result :: term().
with_span(OperationName, Options, Fun) ->
    ErrorIfException = proplists:get_value(error_if_exception, Options, true),
    case ErrorIfException of
        false ->
            try
                start_span(OperationName, Options),
                Fun()
            after
                finish_span()
            end;
        true ->
            try
                start_span(OperationName, Options),
                Fun()
            catch
                Class:Error ?CAPTURE_STACKTRACE ->
                    Stack = ?GET_STACKTRACE,
                    log(#{?LOG_FIELD_ERROR_KIND => Class,
                          ?LOG_FIELD_MESSAGE => Error,
                          ?LOG_FIELD_STACK => Stack},
                        [error]),
                    erlang:raise(Class, Error, Stack)
            after
                finish_span()
            end
    end.

%% @doc Saves `ParentSpan' in the current process dictionary then executes `Fun'.
%%
%% Before returning from the function,
%% `ParentSpan' will be popped from the process dictionary.
-spec with_parent_span(ParentSpan, Fun) -> Result when
      ParentSpan :: {passage:ref_type(), passage:maybe_span()},
      Fun        :: fun (() -> Result),
      Result     :: term().
with_parent_span({_, undefined}, Fun) ->
    push_span(undefined),
    try
        Fun()
    after
        pop_span()
    end;
with_parent_span(ParentSpan, Fun) ->
    push_span(ParentSpan),
    try
        Fun()
    after
        pop_span()
    end.

%% @doc Returns the current span stored in the process dictionary of the calling process.
-spec current_span() -> passage:maybe_span().
current_span() ->
    case get(?ANCESTORS_KEY) of
        [{_, Span} | _] -> Span;
        _               -> undefined
    end.

%% @doc Sets the tracer of the current span to `Tracer'.
%%
%% At the finish of the span, the reporter of `Tracer' will be used to report it.
%%
%% This change affects all descendants of the span.
-spec set_tracer(passage:tracer_id()) -> ok.
set_tracer(Tracer) ->
    update_current_span(
      fun (Span) -> passage_span:set_tracer(Span, Tracer) end).

%% @doc Sets the operation name of the current span to `OperationName'.
-spec set_operation_name(passage:operation_name()) -> ok.
set_operation_name(OperationName) ->
    update_current_span(
      fun (Span) -> passage_span:set_operation_name(Span, OperationName) end).

%% @doc Sets the tags of the current span to `Tags'.
%%
%% Note that the existing tags which have different keys with `Tags' are preserved.
-spec set_tags(Tags) -> ok when
      Tags :: passage:tags() | fun (() -> passage:tags()).
set_tags(Tags) ->
    update_current_span(fun (Span) -> passage:set_tags(Span, Tags) end).

%% @doc Sets the baggage items of the current span to `Items'.
%%
%% Note that the existing items which have different keys with `Items' are preserved.
%%
%% See also: <a href="https://github.com/opentracing/specification/blob/1.1/specification.md#set-a-baggage-item">Set a baggage item (The OpenTracing Semantic Specification)</a>
-spec set_baggage_items(Items) -> ok when
      Items :: passage:baggage_items() | fun (() -> passage:baggage_items()).
set_baggage_items(Items) ->
    update_current_span(fun (Span) -> passage:set_baggage_items(Span, Items) end).

%% @doc Returns the baggage items carried by the current span.
-spec get_baggage_items() -> passage:baggage_items().
get_baggage_items() ->
    Span = current_span(),
    passage:get_baggage_items(Span).

%% @equiv log(Fields, [])
-spec log(Fields) -> ok when
      Fields :: passage:log_fields() | fun (() -> passage:log_fields()).
log(Fields) ->
    log(Fields, []).

%% @doc Logs the `Fields' to the current span.
-spec log(Fields, passage:log_options()) -> ok when
      Fields :: passage:log_fields() | fun (() -> passage:log_fields()).
log(Fields, Options) ->
    update_current_span(fun (Span) -> passage:log(Span, Fields, Options) end).

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
-spec get_ancestors() -> [passage:ref() | undefined].
get_ancestors() ->
    case get(?ANCESTORS_KEY) of
        undefined -> [];
        Ancestors -> Ancestors
    end.

-spec put_ancestors([passage:ref() | undefined]) -> ok.
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
        [{Type, Span0} | Spans] ->
            Span1 = Fun(Span0),
            put_ancestors([{Type, Span1} | Spans])
    end.

-spec push_span(passage:ref() | undefined) -> ok.
push_span(Span) ->
    put_ancestors([Span | get_ancestors()]).

-spec pop_span() -> passage:ref() | undefined.
pop_span() ->
    case get(?ANCESTORS_KEY) of
        undefined      -> undefined;
        []             -> undefined;
        [Span | Spans] ->
            put_ancestors(Spans),
            Span
    end.
