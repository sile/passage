%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
-module(passage_sampler_null_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------
sampler_null_test_() ->
    {foreach,
     fun () -> ok = application:start(passage) end,
     fun (_) -> ok = application:stop(passage) end,
     [
      {"All spans are discarded in sampling phase",
       fun () ->
               Context = passage_span_context_null,
               Sampler = passage_sampler_null:new(),
               Reporter = passage_reporter_null:new(),
               ok = passage_tracer_registry:register(test_tracer, Context, Sampler, Reporter),

               RootSpan = passage:start_span(reporter_null_test, [{tracer, test_tracer}]),
               ?assertEqual(undefined, RootSpan)
       end}
     ]}.
