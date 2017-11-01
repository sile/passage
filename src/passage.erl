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
%% RootSpan = passage:start_span(example_root, [{tracer, tracer}]),
%%
%% %% Starts a child span
%% ChildSpan = passage:start_span(example_child, [{child_of, RootSpan}]),
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

-include("opentracing.hrl").

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([start_span/1, start_span/2]).
-export([finish_span/1, finish_span/2]).
-export([set_tracer/2]).
-export([set_operation_name/2]).
-export([set_tags/2]).
-export([set_baggage_items/2]).
-export([get_baggage_items/1]).
-export([log/2, log/3]).
-export([inject_span/4, extract_span/4]).
-export([strip_span/1]).

-export_type([tracer_id/0]).
-export_type([maybe_span/0]).
-export_type([operation_name/0]).
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

-type ref() :: {ref_type(), passage_span:span()}.
%% Span reference.
%%
%% Note that the values of tags, references and logs of a reference are set to empty
%% when the associated span is created.
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

-type start_span_options() :: [start_span_option()].
%% Options for {@link start_span/2}.

-type start_span_option() :: {tracer, tracer_id()}
                           | {tags, tags()}
                           | {ref_type(), maybe_span()}
                           | {time, erlang:timestamp()}.
%% <ul>
%%   <li><b>tracer</b>: The tracer used for tracing the span. If this option is omitted, the span will never be a root span. If the span has any valid references, this option will be ignored.</li>
%%   <li><b>time</b>: Start timestamp of the span. The default value is `erlang:timestamp()'.</li>
%%   <li><b>tags</b>: Tags associated to the span. The default value is `#{}'.</li>
%%   <li><b>child_of|follows_from</b>: Specifies a references related to the span. This option can be presented more than once.</li>
%% </ul>

-type finish_span_options() :: [finish_span_option()].
%% Options for {@link finish_span/2}.

-type finish_span_option() :: {time, erlang:timestamp()}
                            | {lifetime, pid()}.
%% <ul>
%%   <li><b>time</b>: Finish timestamp of the span. The default value is `erlang:timestamp()'.</li>
%%   <li><b>lifetime</b>: If this option is specified, the report of the finished span will be delayed until the lifetime process exits.</li>
%% </ul>

-type log_options() :: [log_option()].
%% Options for {@link log/3}.

-type log_option() :: {time, erlang:timestamp()}
                    | error | {error, boolean()}.
%% <ul>
%%   <li><b>time</b>: Timestamp of the log. The default value is `erlang:timestamp()'.</li>
%%   <li><b>error</b>:
%%     If this option presents, the log will be treated as an error log.
%%     That is the `event' field with the value `error' will be added automatically.
%%     In addition, the tag `#{error => true}' will be set to the calling span.
%%   </li>
%% </ul>

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
%% @equiv start_span(OperationName, [])
-spec start_span(operation_name()) -> maybe_span().
start_span(OperationName) ->
    start_span(OperationName, []).

%% @doc Starts a span.
%%
%% If any of the following conditions is matched,
%% a valid span object (i.e., non `undefined') will be returned.
%%
%% <ul>
%%   <li>1. There are any valid (i.e., non `undefined') span references.</li>
%%   <li>2. The `sampling.priority' tag exists and the value is a positive integer.</li>
%%   <li>3. A `tracer' is specified and its sampler has determined to sample next span.</li>
%% </ul>
%%
%% If the first condition matches,
%% the tracer associated with the first reference will be used for tracing the resulting span.
-spec start_span(operation_name(), start_span_options()) -> maybe_span().
start_span(OperationName, Options) ->
    Result =
        (fun Recur ([],                        Acc) -> Acc;
             Recur ([{tracer, T}       | L], error) -> Recur(L, {ok, T});
             Recur ([{_, undefined}    | L],   Acc) -> Recur(L, Acc);
             Recur ([{child_of, _}     | _],     _) -> ignore;
             Recur ([{follows_from, _} | _],     _) -> ignore;
             Recur ([_                 | L],   Acc) -> Recur(L, Acc)
         end)(Options, error),
    case Result of
        error        -> undefined;
        ignore       -> passage_span:start(OperationName, Options);
        {ok, Tracer} -> passage_span:start_root(Tracer, OperationName, Options)
    end.

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
    case lists:keyfind(lifetime, 1, Options) of
        false    -> passage_span:finish(Span, Options);
        {_, Pid} ->
            spawn(fun () ->
                          Monitor = monitor(process, Pid),
                          finish_span_when_process_exits(Monitor, Span, Options)
                  end),
            ok
    end.

%% @doc Sets the tracer of `Span' to `Tracer'.
%%
%% At the finish of the span, the reporter of `Tracer' will be used to report it.
%%
%% This change affects all descendants of this span.
-spec set_tracer(maybe_span(), tracer_id()) -> maybe_span().
set_tracer(undefined, _) -> undefined;
set_tracer(Span, Tracer) -> passage_span:set_tracer(Span, Tracer).

%% @doc Sets the operation name of `Span' to `Name'.
-spec set_operation_name(maybe_span(), operation_name()) -> maybe_span().
set_operation_name(undefined, _) -> undefined;
set_operation_name(Span, Name)   -> passage_span:set_operation_name(Span, Name).

%% @doc Sets the tags of `Span' to `Tags'.
%%
%% Note that the existing tags which have different keys with `Tags' are preserved.
-spec set_tags(maybe_span(), Tags) -> maybe_span() when
      Tags :: tags() | fun (() -> tags()).
set_tags(undefined, _)     -> undefined;
set_tags(Span, Tags = #{}) -> passage_span:set_tags(Span, Tags);
set_tags(Span, Fun)        -> set_tags(Span, Fun()).

%% @doc Sets the baggage items of `Span' to `Items'.
%%
%% Note that the existing items which have different keys with `Items' are preserved.
%%
%% See also: <a href="https://github.com/opentracing/specification/blob/1.1/specification.md#set-a-baggage-item">Set a baggage item (The OpenTracing Semantic Specification)</a>
-spec set_baggage_items(maybe_span(), Items) -> maybe_span() when
      Items :: baggage_items() | fun (() -> baggage_items()).
set_baggage_items(undefined, _)      -> undefined;
set_baggage_items(Span, Items = #{}) -> passage_span:set_baggage_items(Span, Items);
set_baggage_items(Span, Fun)         -> set_baggage_items(Span, Fun()).

%% @doc Returns the baggage items carried by `Span'.
-spec get_baggage_items(Span :: maybe_span()) -> baggage_items().
get_baggage_items(undefined) -> #{};
get_baggage_items(Span)      -> passage_span:get_baggage_items(Span).

%% @equiv log(Span, Fields, [])
-spec log(maybe_span(), Fields) -> maybe_span() when
      Fields :: log_fields() | fun (() -> log_fields()).
log(Span, Fields) ->
    log(Span, Fields, []).

%% @doc Logs the `Fields' to `Span'.
-spec log(maybe_span(), Fields, log_options()) -> maybe_span() when
      Fields :: log_fields() | fun (() -> log_fields()).
log(undefined, _, _)                          -> undefined;
log(Span, Fun, Options) when is_function(Fun) -> log(Span, Fun(), Options);
log(Span0, Fields0, Options)                  ->
    case proplists:get_value(error, Options, false) of
        false -> passage_span:log(Span0, Fields0, Options);
        true  ->
            Fields1 = maps:merge(Fields0, #{?LOG_FIELD_EVENT => error}),
            Span1 = passage_span:log(Span0, Fields1, Options),
            passage_span:set_tags(Span1, #{?TAG_ERROR => true})
    end.

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

%% @doc Strips the tags, logs and references from the given span.
-spec strip_span(maybe_span()) -> maybe_span().
strip_span(undefined) -> undefined;
strip_span(Span)      -> passage_span:strip(Span).

%%------------------------------------------------------------------------------
%% Interal Functions
%%------------------------------------------------------------------------------
-spec finish_span_when_process_exits(
        reference(), passage_span:span(), finish_span_options()) -> ok.
finish_span_when_process_exits(Monitor, Span0, Options) ->
    receive
        {'DOWN', Monitor, _, _, Reason} ->
            IsError =
                case Reason of
                    normal        -> false;
                    shutdown      -> false;
                    {shutdown, _} -> false;
                    _             -> true
                end,
            Span1 = log(Span0, #{?LOG_FIELD_EVENT => exit, 'exit.reason' => Reason}),
            Span2 =
                case IsError of
                    false -> Span1;
                    true  -> set_tags(Span1, #{?TAG_ERROR => true})
                end,
            passage_span:finish(Span2, Options);
        _ ->
            finish_span_when_process_exits(Monitor, Span0, Options)
    end.
