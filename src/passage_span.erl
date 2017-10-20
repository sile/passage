-module(passage_span).

-include("opentracing.hrl").

-export([start/2]).
-export([finish/2]).
-export([set_tags/2]).
-export([log/3]).
-export([set_baggage_items/2]).
-export([get_baggage_items/1]).
-export([get_context/1]).
-export([get_tracer/1]).

-export_type([span/0, maybe_span/0]).

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
          finish_time :: erlang:timestamp(),
          refs = [] :: passage:refs(), % TODO: normalize
          tags = #{} :: passage:tags(),
          logs = [] :: [#log{}],
          context :: passage_span_context:maybe_context()
        }).

-opaque span() :: #?SPAN{}.
-type maybe_span() :: span() | undefined.

-spec start(passage:operation_name(), [passage:start_span_option()]) -> maybe_span().
start(OperationName, Options) ->
    StartTime =
        case proplists:get_value(time, Options) of
            undefined -> os:timestamp();
            Time      -> Time
        end,
    Tracer = proplists:get_value(tracer, Options, passage_tracer:default_tracer()),
    Refs = proplists:get_value(refs, Options, []),
    Tags = proplists:get_value(tags, Options, []),
    Span =
        #?SPAN{
            tracer = Tracer,
            operation_name = OperationName,
            start_time = StartTime,
            finish_time = StartTime,
            refs = lists:filter(fun ({_, S}) -> S =/= undefined end, Refs),
            tags = Tags,
            logs = [],
            context = undefined
           },
    case is_sampled(Span) of
        false -> undefined;
        true  ->
            BaggageItems = lists:foldr(fun maps:merge/2, #{}, Refs),
            Context = passage_span_context:make(Span, BaggageItems),
            Span#?SPAN{context = Context}
    end.

-spec finish(maybe_span(), [passage:finish_span_option()]) -> ok.
finish(undefined, _Options) -> ok;
finish(Span0, Options) ->
    FinishTime =
        case proplists:get_value(time, Options) of
            undefined -> os:timestamp();
            Time      -> Time
        end,
    Span1 = Span0#?SPAN{finish_time = FinishTime},
    passage_mailbox:post(Span1).

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

-spec is_sampled(span()) -> boolean().
is_sampled(Span = #?SPAN{tags = Tags, refs = Refs, tracer = Tracer}) ->
    case maps:find(?TAG_SAMPLING_PRIORITY, Tags) of
        {ok, V} -> 0 < V;
        _       ->
            case Refs =/= [] of
                true  -> true;
                false ->
                    Sampler = passage_tracer:get_sampler(Tracer),
                    Name = Span#?SPAN.operation_name,
                    passage_sampler:is_sampled(Sampler, Tracer, Name, Tags)
            end
    end.
