-module(passage_span_context).

-export([make/2]).
-export([get_baggage_items/1]).
-export([set_baggage_items/2]).

-export_type([context/0, maybe_context/0]).

-define(CONTEXT, ?MODULE).

-record(?CONTEXT,
        {
          state :: term(),
          baggage_items = #{} :: passage:baggage_items()
        }).

-opaque context() :: #?CONTEXT{}.

-type maybe_context() :: context() | undefined.

-type format() :: text_map | http_header | binary.

-type carrier() :: term().

-type key() :: binary().
-type value() :: binary().
-type inject_fun() :: fun ((key(), value(), carrier()) -> carrier()).
-type extract_fun() :: fun ((carrier()) -> {ok, key(), value(), carrier()} | error).

-callback make_span_context(passage_span:span(), passage:baggage_items()) ->
    passage_span_context:context().

-callback inject_span_context(passage_span_context:context(), format(),
                              carrier(), inject_fun()) -> carrier().

-callback extract_span_context(format(), carrier(), extract_fun()) ->
    {ok, passage_span_context:context()} | error.

-spec make(passage:tracer_id(), passage_span:normalized_refs()) -> context().
make(Tracer, Refs) ->
    State = passage_tracer:make_span_context_state(Tracer, Refs),
    BaggageItems = lists:foldr(fun maps:merge/2, #{}, Refs),
    #?CONTEXT{ state = State, baggage_items = BaggageItems }.

-spec get_baggage_items(maybe_context()) -> passage:baggage_items().
get_baggage_items(undefined) -> #{};
get_baggage_items(Context)   -> Context#?CONTEXT.baggage_items.

-spec set_baggage_items(maybe_context(), passage:baggage_items()) -> maybe_context().
set_baggage_items(undefined, _BaggageItems) -> undefined;
set_baggage_items(Context,    BaggageItems) ->
    Merged = maps:merge(Context#?CONTEXT.baggage_items, BaggageItems),
    Context#?CONTEXT{baggage_items = Merged}.
