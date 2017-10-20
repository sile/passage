-module(passage_tracer).

-export([default_tracer/0]).
-export([get_sampler/1]).
-export([make_span_context_state/2]).

-callback make_span_context_state(passage_span:span(), passage:baggage_items()) ->
    passage_span_context:state().

%% -callback inject_span_context(passage_span_context:context()) ->
%%     ok.
%% -callback extract_span_context() ->
%%     {ok, passage_span_context:context()} | error.

-spec make_span_context_state(passage_span:span(), passage:baggage_items()) ->
                                     passage_span_context:state().
make_span_context_state(Span, BaggageItems) ->
    Tracer = passage_span:get_tracer(Span),
    Module = passage_registry:get_tracer_module(Tracer),
    Module:make_span_context_state(Span, BaggageItems).

-spec default_tracer() -> passage:tracer_id().
default_tracer() ->
    passage_registry:get_default_tracer().

-spec get_sampler(passage:tracer_id()) -> passage_sampler:sampler().
get_sampler(Tracer) ->
    passage_registry:get_sampler(Tracer).
