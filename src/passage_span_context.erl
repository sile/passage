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

-spec make(passage_span:span(), passage:baggage_items()) -> context().
make(Span, BaggageItems) ->
    State = passage_tracer:make_span_context_state(Span, BaggageItems),
    #?CONTEXT{ state = State, baggage_items = BaggageItems }.

-spec get_baggage_items(maybe_context()) -> passage:baggage_items().
get_baggage_items(undefined) -> #{};
get_baggage_items(Context)   -> Context#?CONTEXT.baggage_items.

-spec set_baggage_items(maybe_context(), passage:baggage_items()) -> maybe_context().
set_baggage_items(undefined, _BaggageItems) -> undefined;
set_baggage_items(Context,    BaggageItems) ->
    Merged = maps:merge(Context#?CONTEXT.baggage_items, BaggageItems),
    Context#?CONTEXT{baggage_items = Merged}.
