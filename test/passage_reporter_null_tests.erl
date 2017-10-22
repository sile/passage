%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
-module(passage_reporter_null_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------
reporter_null_test_() ->
    {foreach,
     fun () -> ok = application:start(passage) end,
     fun (_) -> ok = application:stop(passage) end,
     [
      {"All spans are discarded in reporting phase",
       fun () ->
               Context = passage_span_context_null,
               Sampler = passage_sampler_all:new(),
               Reporter = passage_reporter_null:new(),
               ok = passage_tracer_registry:register(test_tracer, Context, Sampler, Reporter),

               RootSpan = passage:start_root_span(reporter_null_test, test_tracer),
               ?assertNotEqual(undefined, RootSpan),

               passage:finish_span(RootSpan)
       end}
     ]}.
