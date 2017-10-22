%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
-module(passage_tracer_registry_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------
registry_test_() ->
    {foreach,
     fun () -> ok = application:start(passage) end,
     fun (_) -> ok = application:stop(passage) end,
     [
      {"Registration and deregistration",
       fun () ->
               Context = passage_span_context_null,
               Sampler = passage_sampler_null:new(),
               Reporter = passage_reporter_null:new(),

               ?assertEqual(
                  ok, passage_tracer_registry:register(foo, Context, Sampler, Reporter)),
               ?assertEqual([foo], passage_tracer_registry:which_tracers()),

               ?assertEqual(
                  ok, passage_tracer_registry:register(bar, Context, Sampler, Reporter)),
               ?assertEqual([bar, foo], passage_tracer_registry:which_tracers()),

               ?assertEqual(ok, passage_tracer_registry:deregister(foo)),
               ?assertEqual([bar], passage_tracer_registry:which_tracers()),

               ?assertEqual(ok, passage_tracer_registry:deregister(bar)),
               ?assertEqual([], passage_tracer_registry:which_tracers())
       end},
      {"Tracer ID conflict",
       fun () ->
               Context = passage_span_context_null,
               Sampler = passage_sampler_null:new(),
               Reporter = passage_reporter_null:new(),

               ok = passage_tracer_registry:register(foo, Context, Sampler, Reporter),
               ?assertEqual({error, already_registered},
                            passage_tracer_registry:register(foo, Context, Sampler, Reporter)),
               ?assertEqual([foo], passage_tracer_registry:which_tracers())
       end},
      {"Setter and getter",
       fun () ->
               Context = passage_span_context_null,
               Sampler = passage_sampler_null:new(),
               Reporter = passage_reporter_null:new(),

               ok = passage_tracer_registry:register(foo, Context, Sampler, Reporter),

               %% context
               ?assertEqual({ok, Context},
                            passage_tracer_registry:get_span_context_module(foo)),
               ?assertEqual(error,
                            passage_tracer_registry:get_span_context_module(bar)),

               %% sampler
               ?assertEqual({ok, Sampler},
                            passage_tracer_registry:get_sampler(foo)),
               ?assertEqual(error,
                            passage_tracer_registry:get_sampler(bar)),

               OtherSampler = passage_sampler_all:new(),
               ?assertEqual(ok, passage_tracer_registry:set_sampler(foo, OtherSampler)),
               ?assertEqual(ok, passage_tracer_registry:set_sampler(bar, OtherSampler)),

               passage_tracer_registry:which_tracers(), % for synchronization
               ?assertEqual({ok, OtherSampler},
                            passage_tracer_registry:get_sampler(foo)),
               ?assertEqual(error,
                            passage_tracer_registry:get_sampler(bar)),

               %% reporter
               ?assertEqual({ok, Reporter},
                            passage_tracer_registry:get_reporter(foo)),
               ?assertEqual(error,
                            passage_tracer_registry:get_reporter(bar)),

               OtherReporter = passage_reporter_process:new(self(), span),
               ?assertEqual(ok, passage_tracer_registry:set_reporter(foo, OtherReporter)),
               ?assertEqual(ok, passage_tracer_registry:set_reporter(bar, OtherReporter)),

               passage_tracer_registry:which_tracers(), % for synchronization
               ?assertEqual({ok, OtherReporter},
                            passage_tracer_registry:get_reporter(foo)),
               ?assertEqual(error,
                            passage_tracer_registry:get_reporter(bar))
       end}
     ]}.
