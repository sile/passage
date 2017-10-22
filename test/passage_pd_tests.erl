%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
-module(passage_pd_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------
passage_pd_test_() ->
    {foreach,
     fun () -> ok = application:start(passage) end,
     fun (_) -> ok = application:stop(passage) end,
     [
      {"basic test",
       fun () ->
               ok = start_test_tracer(),
               ok = passage_pd:start_root_span(basic_test, test_tracer),
               ok = passage_pd:finish_span(),

               [FinishedSpan] = finished_spans(),
               ?assertEqual(basic_test, passage_span:get_operation_name(FinishedSpan)),
               ?assertEqual([], passage_span:get_refs(FinishedSpan)),
               ?assertEqual([], passage_span:get_logs(FinishedSpan)),
               ?assertEqual(#{}, passage_span:get_tags(FinishedSpan)),
               ?assertEqual(#{}, passage_span:get_baggage_items(FinishedSpan))
       end},
      {"child span",
       fun () ->
               ok = start_test_tracer(),

               ok = passage_pd:start_root_span(root, test_tracer),
               ok = passage_pd:start_span(child),
               ok = passage_pd:finish_span(),
               ok = passage_pd:finish_span(),

               [FinishedChildSpan, FinishedRootSpan] = finished_spans(),
               ?assertEqual(child, passage_span:get_operation_name(FinishedChildSpan)),
               ?assertMatch([{child_of, _}], passage_span:get_refs(FinishedChildSpan)),
               [{child_of, ParentSpan}] = passage_span:get_refs(FinishedChildSpan),
               ?assertEqual(root, passage_span:get_operation_name(ParentSpan)),

               ?assertEqual(root, passage_span:get_operation_name(FinishedRootSpan))
       end},
      {"with span",
       fun () ->
               ok = start_test_tracer(),
               passage_pd:with_root_span(
                 root, test_tracer,
                 fun () ->
                         passage_pd:with_span(child, fun () -> ok end)
                 end),

               [FinishedChildSpan, FinishedRootSpan] = finished_spans(),
               ?assertEqual(child, passage_span:get_operation_name(FinishedChildSpan)),
               ?assertMatch([{child_of, _}], passage_span:get_refs(FinishedChildSpan)),
               [{child_of, ParentSpan}] = passage_span:get_refs(FinishedChildSpan),
               ?assertEqual(root, passage_span:get_operation_name(ParentSpan)),

               ?assertEqual(root, passage_span:get_operation_name(FinishedRootSpan))
       end},
      {"operation name",
       fun () ->
               ok = start_test_tracer(),
               ok = passage_pd:start_root_span(root, test_tracer),
               ok = passage_pd:set_operation_name(foo),

               passage_pd:finish_span(),
               [FinishedSpan] = finished_spans(),
               ?assertEqual(foo, passage_span:get_operation_name(FinishedSpan))
       end},
      {"tag",
       fun () ->
               ok = start_test_tracer(),
               ok = passage_pd:start_root_span(
                      root, test_tracer, [{tags, #{foo => bar, 111 => 222}}]),
               ok = passage_pd:set_tags(#{baz => qux, 111 => 333}),

               ok = passage_pd:finish_span(),
               [FinishedSpan] = finished_spans(),
               ?assertEqual(#{foo => bar, baz => qux, 111 => 333},
                            passage_span:get_tags(FinishedSpan))
       end},
      {"baggage item",
       fun () ->
               ok = start_test_tracer(),
               ok = passage_pd:start_root_span(root, test_tracer),
               ok = passage_pd:set_baggage_items(#{<<"foo">> => <<"bar">>}),

               ok = passage_pd:start_span(child),
               ?assertEqual(#{<<"foo">> => <<"bar">>}, passage_pd:get_baggage_items()),

               ok = passage_pd:set_baggage_items(#{<<"baz">> => <<"qux">>}),
               ?assertEqual(#{<<"foo">> => <<"bar">>, <<"baz">> => <<"qux">>},
                            passage_pd:get_baggage_items())
       end},
      {"log",
       fun () ->
               ok = start_test_tracer(),
               ok = passage_pd:start_root_span(root, test_tracer),
               ok = passage_pd:log(#{hello => world}, [{time, {1, 2, 3}}]),
               ok = passage_pd:log(#{foo => bar}),

               ok = passage_pd:finish_span(),
               [FinishedSpan] = finished_spans(),
               ?assertMatch([{#{foo := bar}, {_, _, _}},
                             {#{hello := world}, {1, 2, 3}}],
                            passage_span:get_logs(FinishedSpan))
       end},
      {"error log",
       fun () ->
               ok = start_test_tracer(),
               ok = passage_pd:start_root_span(root, test_tracer),
               ok = passage_pd:error_log("Hello World", [], #{kind => greeting}),

               ok = passage_pd:finish_span(),
               [FinishedSpan] = finished_spans(),
               ?assertMatch([{#{event := error, kind := greeting, message := "Hello World"},
                              {_, _, _}}],
                            passage_span:get_logs(FinishedSpan)),
               ?assertEqual(#{error => true},
                            passage_span:get_tags(FinishedSpan))
       end},
      {"'sampling.priority' = 0",
       fun () ->
               ok = start_test_tracer(),

               ok = passage_pd:start_root_span(
                      basic_test, test_tracer, [{tags, #{'sampling.priority' => 0}}]),
               ok = passage_pd:finish_span(),
               ?assertEqual([], finished_spans())
       end},
      {"'sampling.priority' = 1",
       fun () ->
               Context = passage_span_context_null,
               Sampler = passage_sampler_null:new(),
               Reporter = passage_reporter_process:new(self(), span),
               ok = passage_tracer_registry:register(
                      test_tracer, Context, Sampler, Reporter),

               ok = passage_pd:start_root_span(
                      basic_test, test_tracer, [{tags, #{'sampling.priority' => 1}}]),
               ok = passage_pd:finish_span(),
               ?assertMatch([_Span], finished_spans())
       end},
      {"unsampled parent",
       fun () ->
               ok = passage_pd:start_span(child),
               ok = passage_pd:set_operation_name(foo),
               ok = passage_pd:set_tags(#{111 => 222}),
               ok = passage_pd:set_baggage_items(#{<<"a">> => <<"b">>}),
               ok = passage_pd:log(#{event => debug}),

               ok = passage_pd:finish_span(),
               ?assertEqual([], finished_spans())
       end},
      {"additional references",
       fun () ->
               ok = start_test_tracer(),
               RootSpan = passage:start_root_span(root, test_tracer),

               ok = passage_pd:start_span(child, [{refs, [{follows_from, RootSpan}]}]),
               ok = passage_pd:finish_span(),
               ?assertMatch([_Span], finished_spans())
       end}
      ]}.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
-spec start_test_tracer() -> ok.
start_test_tracer() ->
    Context = passage_span_context_null,
    Sampler = passage_sampler_all:new(),
    Reporter = passage_reporter_process:new(self(), span),
    ok = passage_tracer_registry:register(test_tracer, Context, Sampler, Reporter).

-spec finished_spans() -> [passage_span:span()].
finished_spans() ->
    receive
        {span, Span} -> [Span] ++ finished_spans()
    after 0 ->
            []
    end.
