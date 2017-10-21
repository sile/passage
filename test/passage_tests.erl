-module(passage_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    Context = passage_span_context_null,
    Sampler = passage_sampler_null:new(),
    Reporter = passage_reporter_null:new(),
    ok = passage_tracer:start(test_tracer, Context, Sampler, Reporter),
    ok.
