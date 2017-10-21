%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%%
-module(passage_span).

-include("opentracing.hrl").

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export_type([span/0]).
-export_type([normalized_refs/0, normalized_ref/0]).

%%
-export([set_operation_name/2]).
-export([set_tags/2]).
-export([log/3]).
-export([set_baggage_items/2]).
-export([get_baggage_items/1]).
-export([get_context/1]).
-export([get_tracer/1]).

%%------------------------------------------------------------------------------
%% Application Internal API
%%------------------------------------------------------------------------------
-export([make_extracted_span/2]).
-export([start/2, start_root/3]).
-export([finish/2]).

%%------------------------------------------------------------------------------
%% Macros & Records
%%------------------------------------------------------------------------------
-define(SPAN, ?MODULE).

-record(log,
        {
          fields :: passage:log_fields(),
          time   :: erlang:timestamp()
        }).

-record(?SPAN,
        {
          tracer :: passage:tracer_id(),
          operation_name :: passage:operation_name(),
          start_time :: erlang:timestamp(),
          finish_time = undefined :: erlang:timestamp() | undefined,
          refs = [] :: normalized_refs(),
          tags = #{} :: passage:tags(),
          logs = [] :: [#log{}],
          context :: passage_span_context:maybe_context()
        }).

%%------------------------------------------------------------------------------
%% Exported Types
%%------------------------------------------------------------------------------
-opaque span() :: #?SPAN{}.

-type normalized_refs() :: [normalized_ref()].

-type normalized_ref() :: {passage:ref_type(), span()}.

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------

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
      Options :: passage:start_root_span_options().
start_root(Tracer, OperationName, Options) ->
    Tags = proplists:get_value(tags, Options, #{}),
    case is_sampled(Tracer, OperationName, Tags) of
        false -> undefined;
        true  ->
            Context = passage_span_context:make(Tracer, []),
            StartTime =
                case lists:keyfind(time, 1, Options) of
                    false     -> os:timestamp();
                    {_, Time} -> Time
                end,
            #?SPAN{
                tracer         = Tracer,
                operation_name = OperationName,
                start_time     = StartTime,
                tags           = Tags,
                context        = Context
               }
    end.

%% @private
-spec start(passage:operation_name(), passage:start_span_options()) -> passage:maybe_span().
start(OperationName, Options) ->
    Refs = lists:keydelete(undefined, 2, proplists:get_value(refs, Options, [])),
    case Refs of
        []                 -> undefined;
        [{_, Primary} | _] ->
            Tracer = get_tracer(Primary),
            Context = passage_span_context:make(Tracer, Refs),
            Tags = proplists:get_value(tags, Options, #{}),
            StartTime =
                case lists:keyfind(time, 1, Options) of
                    false     -> os:timestamp();
                    {_, Time} -> Time
                end,
            #?SPAN{
                tracer         = Tracer,
                operation_name = OperationName,
                start_time     = StartTime,
                refs           = Refs,
                tags           = Tags,
                context        = Context
               }
    end.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

-spec is_sampled(passage:tracer_id(), passage:operation_name(), passage:tags()) -> boolean().
is_sampled(Tracer, OperationName, Tags) ->
    case maps:find(?TAG_SAMPLING_PRIORITY, Tags) of
        {ok, V} -> 0 < V;
        _       ->
            Sampler = passage_tracer:get_sampler(Tracer),
            passage_sampler:is_sampled(Sampler, OperationName, Tags)
    end.

-type maybe_span() :: term().

%% @private
-spec finish(span(), passage:finish_span_options()) -> ok.
finish(#?SPAN{operation_name = undefined, start_time = {0, 0, 0}}, _) ->
    %% This is an extracted span (ignored).
    ok;
finish(Span0, Options) ->
    FinishTime =
        case proplists:get_value(time, Options) of
            undefined -> os:timestamp();
            Time      -> Time
        end,
    Span1 = Span0#?SPAN{finish_time = FinishTime},
    Reporter = passage_registry:get_reporter(Span1#?SPAN.tracer),
    passage_reporter:report(Reporter, Span1).

-spec set_operation_name(span(), passage:operation_name()) -> span().
set_operation_name(Span, OperationName) ->
    Span#?SPAN{operation_name = OperationName}.

-spec set_tags(maybe_span(), passage:tags()) -> maybe_span().
set_tags(undefined, _Tags) -> undefined;
set_tags(Span,       Tags) -> Span#?SPAN{tags = maps:merge(Span#?SPAN.tags, Tags)}.

-spec log(maybe_span(), passage:log_fields(), [passage:log_option()]) -> maybe_span().
log(undefined, _Fields, _Options) -> undefined;
log(Span, Fields, Options) ->
    Time =
        case proplists:get_value(time, Options) of
            undefined -> os:timestamp();
            Timestamp -> Timestamp
        end,
    Log = #log{fields = Fields, time = Time},
    Span#?SPAN{logs = [Log | Span#?SPAN.logs]}.

-spec set_baggage_items(maybe_span(), passage:baggage_items()) -> maybe_span().
set_baggage_items(Span, Items) ->
    Span#?SPAN{context = passage_span_context:set_baggage_items(get_context(Span), Items)}.

-spec get_baggage_items(maybe_span()) -> passage:baggage_items().
get_baggage_items(Span) ->
    passage_span_context:get_baggage_items(get_context(Span)).

-spec get_context(maybe_span()) -> passage_span_context:maybe_context().
get_context(undefined) -> undefined;
get_context(Span)      -> Span#?SPAN.context.

%% TODO: `span()` or `maybe_span()`
-spec get_tracer(span()) -> passage:tracer_id().
get_tracer(Span) ->
    Span#?SPAN.tracer.
