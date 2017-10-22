%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc API for operating active spans.
%%
%% Most of the functions and types provided in this module are based on the
%% <a href="https://github.com/opentracing/specification/blob/1.1/specification.md">OpenTracing API</a>.
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
%% RootSpan = passage:start_root_span(example_root, tracer),
%%
%% %% Starts a child span
%% ChildSpan = passage:start_span(example_child, {child_of, RootSpan}),
%%
%% %% Finishes spans
%% passage:finish_span(ChildSpan),
%% passage:finish_span(RootSpan),
%%
%% %% Receives the finished spans
%% receive {span, FinishedChildSpan} -> ok end,
%% receive {span, FinishedRootSpan} -> ok end.
%% '''
-module(passage).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([start_root_span/2, start_root_span/3]).
-export([start_span/2, start_span/3]).
-export([finish_span/1, finish_span/2]).
-export([set_operation_name/2]).
-export([set_tags/2]).
-export([set_baggage_items/2]).
-export([get_baggage_items/1]).
-export([log/2, log/3]).
-export([error_log/2, error_log/3, error_log/4, error_log/5]).
-export([inject_span/4, extract_span/4]).

-export_type([tracer_id/0]).
-export_type([maybe_span/0]).
-export_type([operation_name/0]).
-export_type([start_root_span_option/0, start_root_span_options/0]).
-export_type([start_span_option/0, start_span_options/0]).
-export_type([finish_span_option/0, finish_span_options/0]).
-export_type([tags/0, tag_name/0, tag_value/0]).
-export_type([refs/0, ref/0, ref_type/0]).
-export_type([baggage_items/0, baggage_item_name/0, baggage_item_value/0]).
-export_type([log_fields/0, log_field_name/0, log_field_value/0]).
-export_type([log_option/0, log_options/0]).

%%------------------------------------------------------------------------------
%% Exported Types
%%------------------------------------------------------------------------------
-type tracer_id() :: atom().
%% Tracer identifier.

-type maybe_span() :: passage_span:span() | undefined.
%% Sampled or unsampled span.
%%
%% `undefined' means the span is unsampled.

-type operation_name() :: atom().
%% Operation name.

-type refs() :: [ref()].
%% Span references.

-type ref() :: {ref_type(), maybe_span()}.
%% Span reference.
%%
%% See also: <a href="https://github.com/opentracing/specification/blob/1.1/specification.md#references-between-spans">References between Spans (The OpenTracing Semantic Specification)</a>

-type ref_type() :: child_of | follows_from.
%% Span reference type.

-type tags() :: #{tag_name() => tag_value()}.
%% Span tags.
%%
%% See also: <a href="https://github.com/opentracing/specification/blob/1.1/semantic_conventions.md#span-tags-table">Standard span tags (OpenTracing API)</a>

-type tag_name() :: atom().
%% Tag name.

-type tag_value() :: term().
%% Tag value.

-type baggage_items() :: #{baggage_item_name() => baggage_item_value()}.
%% Baggage items.
%%
%% Baggage items are just key/value pairs that cross OS process boundaries.

-type baggage_item_name() :: binary().
%% Baggage item name.

-type baggage_item_value() :: binary().
%% Baggage item value.

-type log_fields() :: #{log_field_name() => log_field_value()}.
%% Span log fields.
%%
%% See also: <a href="https://github.com/opentracing/specification/blob/1.1/semantic_conventions.md#log-fields-table">Standard log fields (OpenTracing API)</a>

-type log_field_name() :: atom().
%% Log field name.

-type log_field_value() :: term().
%% Log field value.

-type start_root_span_options() :: [start_root_span_option()].
%% Options for {@link start_root_span/3}.

-type start_root_span_option() :: {time, erlang:timestamp()}
                                | {tags, tags()}.
%% <ul>
%%  <li><b>time</b>: Start timestamp of the span. The default value is `erlang:timestamp()'.</li>
%%  <li><b>tags</b>: Tags associated to the span. The default value is `#{}'.</li>
%% </ul>

-type start_span_options() :: [start_span_option()].
%% Options for {@link start_span/3}.

-type start_span_option() :: {refs, refs()}
                           | start_root_span_option().
%% <ul>
%%   <li><b>refs</b>: Additional references related to the span. The default value is `[]'. </li>
%% </ul>

-type finish_span_options() :: [finish_span_option()].
%% Options for {@link finish_span/2}.

-type finish_span_option() :: {time, erlang:timestamp()}.
%% <ul>
%%   <li><b>time</b>: Finish timestamp of the span. The default value is `erlang:timestamp()'.</li>
%% </ul>

-type log_options() :: [log_option()].
%% Options for {@link log/3}.

-type log_option() :: {time, erlang:timestamp()}.
%% <ul>
%%   <li><b>time</b>: Timestamp of the log. The default value is `erlang:timestamp()'.</li>
%% </ul>
%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
%% @equiv start_root_span(OperationName, Tracer, [])
-spec start_root_span(operation_name(), tracer_id()) -> maybe_span().
start_root_span(OperationName, Tracer) ->
    start_root_span(OperationName, Tracer, []).

%% @doc Starts a root span.
%%
%% If the sampler associated with `Tracer' does not sample the span,
%% this function will return `undefined'.
-spec start_root_span(operation_name(), tracer_id(), start_root_span_options()) ->
                             maybe_span().
start_root_span(OperationName, Tracer, Options) ->
    passage_span:start_root(Tracer, OperationName, Options).

%% @equiv start_span(OperationName, PrimaryReference, [])
-spec start_span(operation_name(), ref()) -> maybe_span().
start_span(OperationName, PrimaryReference) ->
    start_span(OperationName, PrimaryReference, []).

%% @doc Starts a span.
%%
%% If there is no sampled span references, this function will return `undefined'.
-spec start_span(operation_name(), ref(), start_span_options()) -> maybe_span().
start_span(OperationName, PrimaryReference, Options) ->
    {Refs1, Options2} =
        case lists:keytake(refs, 1, Options) of
            false                            -> {[], Options};
            {value, {refs, Refs0}, Options1} -> {Refs0, Options1}
        end,
    Options3 = [{refs, [PrimaryReference | Refs1]} | Options2],
    passage_span:start(OperationName, Options3).

%% @equiv finish_span(Span, [])
-spec finish_span(maybe_span()) -> ok.
finish_span(Span) ->
    finish_span(Span, []).

%% @doc Finishes the span.
%%
%% The finished span will be sent an external observer via
%% the reporter associated with the tracer of the span.
%%
%% Note that if you call this function on the same span more than once,
%% duplicate reports will be sent.
-spec finish_span(maybe_span(), finish_span_options()) -> ok.
finish_span(undefined, _)  -> ok;
finish_span(Span, Options) ->
    passage_span:finish(Span, Options).

%% @doc Sets the operation name of `Span' to `Name'.
-spec set_operation_name(maybe_span(), operation_name()) -> maybe_span().
set_operation_name(undefined, _) -> undefined;
set_operation_name(Span, Name)   -> passage_span:set_operation_name(Span, Name).

%% @doc Sets the tags of `Span' to `Tags'.
%%
%% Note that the existing tags which have different keys with `Tags' are preserved.
-spec set_tags(maybe_span(), tags()) -> maybe_span().
set_tags(undefined, _) -> undefined;
set_tags(Span, Tags)   -> passage_span:set_tags(Span, Tags).

%% @doc Sets the baggage items of `Span' to `Items'.
%%
%% Note that the existing items which have different keys with `Items' are preserved.
%%
%% See also: <a href="https://github.com/opentracing/specification/blob/1.1/specification.md#set-a-baggage-item">Set a baggage item (The OpenTracing Semantic Specification)</a>
-spec set_baggage_items(maybe_span(), baggage_items()) -> maybe_span().
set_baggage_items(undefined, _) -> undefined;
set_baggage_items(Span, Items)  -> passage_span:set_baggage_items(Span, Items).

%% @doc Returns the baggage items carried by `Span'.
-spec get_baggage_items(Span :: maybe_span()) -> baggage_items().
get_baggage_items(undefined) -> #{};
get_baggage_items(Span)      -> passage_span:get_baggage_items(Span).

%% @equiv log(Span, Fields, [])
-spec log(maybe_span(), log_fields()) -> maybe_span().
log(Span, Fields) ->
    log(Span, Fields, []).

%% @doc Logs the `Fields' to `Span'.
-spec log(maybe_span(), log_fields(), log_options()) -> maybe_span().
log(undefined, _, _)       -> undefined;
log(Span, Fields, Options) -> passage_span:log(Span, Fields, Options).

%% @equiv error_log(Span, Message, [])
-spec error_log(maybe_span(), iodata()) -> maybe_span().
error_log(Span, Message) ->
    error_log(Span, Message, []).

%% @equiv error_log(Span, Format, Data, #{})
-spec error_log(maybe_span(), io:format(), [term()]) -> maybe_span().
error_log(Span, Format, Data) ->
    error_log(Span, Format, Data, #{}).

%% @equiv error_log(Span, Format, Data, Fields, [])
-spec error_log(maybe_span(), io:format(), [term()], log_fields()) -> maybe_span().
error_log(Span, Format, Data, Fields) ->
    error_log(Span, Format, Data, Fields, []).

%% @doc Logs error message to `Span'.
%%
%% This function logs `Fields` and
%% `#{event => error, message => io_lib:format(Format, Data)}'.
%%
%% In addition, it sets the tag `#{error => true}' automatically.
-spec error_log(maybe_span(), io:format(), [term()], log_fields(), log_options()) ->
                       maybe_span().
error_log(undefined, _, _, _, _)               -> undefined;
error_log(Span0, Format, Data, Fields, Options) ->
    Message = io_lib:format(Format, Data),
    Fields1 = maps:merge(Fields, #{event => error, message => Message}),
    Span1 = passage_span:log(Span0, Fields1, Options),
    passage_span:set_tags(Span1, #{error => true}).

%% @doc Injects `Span' into `Carrier'.
-spec inject_span(Span, Format, InjectFun, Carrier) -> Carrier when
      Span :: maybe_span(),
      Format :: passage_span_context:format(),
      InjectFun :: passage_span_context:inject_fun(),
      Carrier :: passage_span_context:carrier().
inject_span(undefined, _, _, Carrier)         -> Carrier;
inject_span(Span, Format, InjectFun, Carrier) ->
    Context = passage_span:get_context(Span),
    Tracer = passage_span:get_tracer(Span),
    case passage_tracer_registry:get_span_context_module(Tracer) of
        error        -> Carrier;
        {ok, Module} -> Module:inject_span_context(Context, Format, InjectFun, Carrier)
    end.

%% @doc Extracts a span from `Carrier'.
%%
%% If `Carrier' has no span context, this function will return `undefined'.
-spec extract_span(Tracer, Format, IterateFun, Carrier) -> maybe_span() when
      Tracer :: tracer_id(),
      Format :: passage_span_context:format(),
      IterateFun :: passage_span_context:iterate_fun(),
      Carrier :: passage_span_context:carrier().
extract_span(Tracer, Format, IterateFun, Carrier) ->
    case passage_tracer_registry:get_span_context_module(Tracer) of
        error        -> undefined;
        {ok, Module} ->
            case Module:extract_span_context(Format, IterateFun, Carrier) of
                error         -> undefined;
                {ok, Context} -> passage_span:make_extracted_span(Tracer, Context)
            end
    end.
