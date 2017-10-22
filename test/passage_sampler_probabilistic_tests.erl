%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
-module(passage_sampler_probabilistic_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------
sampler_probabilistic_test_() ->
    {foreach,
     fun () -> ok = application:start(passage) end,
     fun (_) -> ok = application:stop(passage) end,
     [
      {"All spans are discarded",
       fun () ->
               Context = passage_span_context_null,
               Sampler = passage_sampler_probabilistic:new(0.0),
               Reporter = passage_reporter_null:new(),
               ok = passage_tracer_registry:register(test_tracer, Context, Sampler, Reporter),

               RootSpan = passage:start_root_span(reporter_null_test, test_tracer),
               ?assertEqual(undefined, RootSpan)
       end},
      {"All spans are sampled",
       fun () ->
               Context = passage_span_context_null,
               Sampler = passage_sampler_probabilistic:new(1.0),
               Reporter = passage_reporter_null:new(),
               ok = passage_tracer_registry:register(test_tracer, Context, Sampler, Reporter),

               RootSpan = passage:start_root_span(reporter_null_test, test_tracer),
               ?assertNotEqual(undefined, RootSpan)
       end},
      {"Wrong argument",
       fun () ->
               ?assertError(badarg, passage_sampler_probabilistic:new(-1)),
               ?assertError(badarg, passage_sampler_probabilistic:new(1.1)),
               ?assertError(badarg, passage_sampler_probabilistic:new(abc))
       end}
     ]}.
