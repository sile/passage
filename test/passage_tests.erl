-module(passage_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    {ok, _} = application:ensure_all_started(passage),

    Context = passage_span_context_null,
    Sampler = passage_sampler_all:new(),
    Reporter = passage_reporter_process:new(self(), span),
    ok = passage_tracer_registry:register(test_tracer, Context, Sampler, Reporter),

    ?assertEqual([test_tracer], passage_tracer_registry:which_tracers()),

    RootSpan = passage:start_root_span(basic_test, test_tracer),
    passage:finish_span(RootSpan),

    receive
        {span, FinishedSpan} ->
            ?assertEqual(passage_span:get_operation_name(RootSpan),
                         passage_span:get_operation_name(FinishedSpan))
    end,

    ok = application:stop(passage).
