%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Span.
%%
%% <blockquote>
%% <b>Traces</b> in OpenTracing are defined implicitly by their <b>Spans</b>.
%% In particular, a <b>Trace</b> can be thought of as a directed acyclic graph (DAG) of <b>Spans</b>,
%% where the edges between <b>Spans</b> are called <b>References</b>.
%%
%% Each <b>Span</b> encapsulates the following state:
%% <ul>
%%   <li>An operation name</li>
%%   <li>A start timestamp</li>
%%   <li>A finish timestamp</li>
%%   <li>A set of zero or more key:value <b>Span Tags</b>. The keys must be strings. The values may be strings, bools, or numeric types.</li>
%%   <li>A set of zero or more <b>Span Logs</b>, each of which is itself a key:value map paired with a timestamp. The keys must be strings, though the values may be of any type. Not all OpenTracing implementations must support every value type.</li>
%%   <li>A <b>SpanContext</b></li>
%%   <li><b>References</b> to zero or more causally-related <b>Spans</b> (via the <b>SpanContext</b> of those related <b>Spans</b>)</li>
%% </ul>
%%
%% <a href="https://github.com/opentracing/specification/blob/1.1/specification.md#the-opentracing-data-model">
%% The OpenTracing Data Model
%% </a>
%% </blockquote>
%%
%% === Examples ===
%%
%% ```
%% %% Registers a tracer
%% Context = passage_span_context_null,
%% Sampler = passage_sampler_all:new(),
%% Reporter = passage_reporter_null:new(),
%% ok = passage_tracer_registry:register(tracer, Context, Sampler, Reporter),
%%
%% %% Starts a span
%% MaybeSpan = passage:start_span(example, [{tracer, tracer}]),
%% case MaybeSpan of
%%     undefined -> ok;
%%     Span      -> example = passage_span:get_operation_name(Span)
%% end,
%%
%% %% Finishes a span
%% passage:finish_span(MaybeSpan).
%% '''
-module(passage_span).

-include("opentracing.hrl").

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([get_operation_name/1]).
-export([get_tags/1]).
-export([get_refs/1]).
-export([get_logs/1]).
-export([get_start_time/1]).
-export([get_finish_time/1]).
-export([get_context/1]).

-export_type([span/0]).
-export_type([log/0]).

%%------------------------------------------------------------------------------
%% Application Internal API
%%------------------------------------------------------------------------------
-export([make_extracted_span/2]).
-export([start/2, start_root/3]).
-export([finish/2]).
-export([set_operation_name/2]).
-export([set_tags/2]).
-export([log/3]).
-export([set_baggage_items/2]).
-export([get_baggage_items/1]).
-export([get_tracer/1]).

%%------------------------------------------------------------------------------
%% Macros & Records
%%------------------------------------------------------------------------------
-define(SPAN, ?MODULE).

-record(?SPAN,
        {
          tracer                  :: passage:tracer_id(),
          operation_name          :: passage:operation_name(),
          start_time              :: erlang:timestamp(),
          finish_time = undefined :: erlang:timestamp() | undefined,
          refs        = []        :: passage:refs(),
          tags        = #{}       :: passage:tags(),
          logs        = []        :: [log()],
          context                 :: passage_span_context:context()
        }).

%%------------------------------------------------------------------------------
%% Exported Types
%%------------------------------------------------------------------------------
-opaque span() :: #?SPAN{}.
%% Span.

-type log() :: {passage:log_fields(), erlang:timestamp()}.
%% Timestamped span log fields.

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
%% @doc Returns the operation name of `Span'.
-spec get_operation_name(span()) -> passage:operation_name().
get_operation_name(Span) ->
    Span#?SPAN.operation_name.

%% @doc Returns the tags of `Span'.
-spec get_tags(span()) -> passage:tags().
get_tags(Span) ->
    Span#?SPAN.tags.

%% @doc Returns the references of `Span'.
-spec get_refs(span()) -> passage:refs().
get_refs(Span) ->
    Span#?SPAN.refs.

%% @doc Returns the logs of `Span'.
-spec get_logs(span()) -> [log()].
get_logs(Span) ->
    Span#?SPAN.logs.

%% @doc Returns the start time of `Span'.
-spec get_start_time(span()) -> erlang:timestamp().
get_start_time(Span) ->
    Span#?SPAN.start_time.

%% @doc Returns the finish time of `Span'.
%%
%% If the span has not been finished, it will return `error'.
-spec get_finish_time(Span :: span()) -> {ok, erlang:timestamp()} | error.
get_finish_time(#?SPAN{finish_time = undefined}) ->
    error;
get_finish_time(Span) ->
    {ok, Span#?SPAN.finish_time}.

%% @doc Returns the context of `Span'.
-spec get_context(span()) -> passage_span_context:context().
get_context(Span) ->
    Span#?SPAN.context.

%%------------------------------------------------------------------------------
%% Application Internal Functions
%%------------------------------------------------------------------------------
%% @private
-spec make_extracted_span(passage:tracer_id(), passage_span_context:context()) -> span().
make_extracted_span(Tracer, Context) ->
    #?SPAN{
        tracer         = Tracer,
        operation_name = undefined,
        start_time     = {0, 0, 0},
        context        = Context
       }.

%% @private
-spec start_root(passage:tracer_id(), passage:operation_name(), Options) ->
                        passage:maybe_span() when
      Options :: passage:start_span_options().
start_root(Tracer, OperationName, Options) ->
    Tags = proplists:get_value(tags, Options, #{}),
    case is_sampled(Tracer, OperationName, Tags) of
        false -> undefined;
        true  ->
            case make_span_context(Tracer, []) of
                error         -> undefined;
                {ok, Context} ->
                    StartTime = get_time(Options),
                    #?SPAN{
                        tracer         = Tracer,
                        operation_name = OperationName,
                        start_time     = StartTime,
                        tags           = Tags,
                        context        = Context
                       }
            end
    end.

%% @private
-spec start(passage:operation_name(), passage:start_span_options()) -> passage:maybe_span().
start(OperationName, Options) ->
    Refs = collect_valid_refs(Options),
    case Refs of
        []                 -> undefined;
        [{_, Primary} | _] ->
            Tracer = get_tracer(Primary),
            case make_span_context(Tracer, Refs) of
                error         -> undefined;
                {ok, Context} ->
                    Tags = proplists:get_value(tags, Options, #{}),
                    StartTime = get_time(Options),
                    #?SPAN{
                        tracer         = Tracer,
                        operation_name = OperationName,
                        start_time     = StartTime,
                        refs           = Refs,
                        tags           = Tags,
                        context        = Context
                       }
            end
    end.

%% @private
-spec finish(span(), passage:finish_span_options()) -> ok.
finish(#?SPAN{operation_name = undefined, start_time = {0, 0, 0}}, _) ->
    %% This is an extracted span (ignored).
    ok;
finish(Span0, Options) ->
    FinishTime = get_time(Options),
    Span1 = Span0#?SPAN{finish_time = FinishTime},
    case passage_tracer_registry:get_reporter(Span1#?SPAN.tracer) of
        error          -> ok;
        {ok, Reporter} -> passage_reporter:report(Reporter, Span1)
    end.

%% @private
-spec set_operation_name(span(), passage:operation_name()) -> span().
set_operation_name(Span, OperationName) ->
    Span#?SPAN{operation_name = OperationName}.

%% @private
-spec set_tags(span(), passage:tags()) -> span().
set_tags(Span, Tags) ->
    Span#?SPAN{tags = maps:merge(Span#?SPAN.tags, Tags)}.

%% @private
-spec log(span(), passage:log_fields(), passage:log_options()) -> span().
log(Span, Fields, Options) ->
    Time = get_time(Options),
    Span#?SPAN{logs = [{Fields, Time} | Span#?SPAN.logs]}.

%% @private
-spec set_baggage_items(span(), passage:baggage_items()) -> span().
set_baggage_items(Span, Items) ->
    Context = Span#?SPAN.context,
    Span#?SPAN{context = passage_span_context:set_baggage_items(Context, Items)}.

%% @private
-spec get_baggage_items(span()) -> passage:baggage_items().
get_baggage_items(Span) ->
    passage_span_context:get_baggage_items(Span#?SPAN.context).

%% @private
-spec get_tracer(span()) -> passage:tracer_id().
get_tracer(Span) ->
    Span#?SPAN.tracer.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
-spec is_sampled(passage:tracer_id(), passage:operation_name(), passage:tags()) -> boolean().
is_sampled(Tracer, OperationName, Tags) ->
    case maps:find(?TAG_SAMPLING_PRIORITY, Tags) of
        {ok, V} -> 0 < V;
        _       ->
            case passage_tracer_registry:get_sampler(Tracer) of
                error         -> false;
                {ok, Sampler} -> passage_sampler:is_sampled(Sampler, OperationName, Tags)
            end
    end.

-spec get_time([{time, erlang:timestamp()}]) -> erlang:timestamp().
get_time(Options) ->
    case lists:keyfind(time, 1, Options) of
        false     -> erlang:timestamp();
        {_, Time} -> Time
    end.

-spec make_span_context(passage:tracer_id(), passage:refs()) ->
                               {ok, passage_span_context:context()} | error.
make_span_context(Tracer, Refs) ->
    case passage_tracer_registry:get_span_context_module(Tracer) of
        error        -> error;
        {ok, Module} -> {ok, passage_span_context:from_refs(Module, Refs)}
    end.

-spec collect_valid_refs(passage:start_span_options()) -> passage:refs().
collect_valid_refs(Options) ->
    lists:filtermap(
      fun ({_, undefined})    -> false;
          ({child_of, S})     -> {true, {child_of, strip(S)}};
          ({follows_from, S}) -> {true, {follows_from, strip(S)}};
          (_)                 -> false
      end,
      Options).

-spec strip(span()) -> span().
strip(Span) ->
    Span#?SPAN{refs = [], tags = #{}, logs = []}.
