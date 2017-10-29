%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
-module(passage_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------
passage_test_() ->
    {foreach,
     fun () -> ok = application:start(passage) end,
     fun (_) -> ok = application:stop(passage) end,
     [
      {"basic test",
       fun () ->
               ok = start_test_tracer(),
               ?assertEqual([test_tracer], passage_tracer_registry:which_tracers()),

               RootSpan = passage:start_span(basic_test, [{tracer, test_tracer}]),
               passage:finish_span(RootSpan),

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

               RootSpan = passage:start_span(root, [{tracer, test_tracer}]),
               ChildSpan = passage:start_span(child, [{child_of, RootSpan}]),
               passage:finish_span(ChildSpan),
               passage:finish_span(RootSpan),

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
               Span0 = passage:start_span(root, [{tracer, test_tracer}]),
               Span1 = passage:set_operation_name(Span0, foo),

               passage:finish_span(Span1),
               [FinishedSpan] = finished_spans(),
               ?assertEqual(foo, passage_span:get_operation_name(FinishedSpan))
       end},
      {"tag",
       fun () ->
               ok = start_test_tracer(),
               Span0 =
                   passage:start_span(
                     root, [{tracer, test_tracer}, {tags, #{foo => bar, 111 => 222}}]),
               Span1 = passage:set_tags(Span0, #{baz => qux, 111 => 333}),

               passage:finish_span(Span1),
               [FinishedSpan] = finished_spans(),
               ?assertEqual(#{foo => bar, baz => qux, 111 => 333},
                            passage_span:get_tags(FinishedSpan))
       end},
      {"tag function",
       fun () ->
               ok = start_test_tracer(),
               Span0 =
                   passage:start_span(
                     root, [{tracer, test_tracer}, {tags, #{foo => bar, 111 => 222}}]),
               Span1 = passage:set_tags(Span0, fun () -> #{baz => qux, 111 => 333} end),

               passage:finish_span(Span1),
               [FinishedSpan] = finished_spans(),
               ?assertEqual(#{foo => bar, baz => qux, 111 => 333},
                            passage_span:get_tags(FinishedSpan))
       end},
      {"baggage item",
       fun () ->
               ok = start_test_tracer(),
               RootSpan0 = passage:start_span(root, [{tracer, test_tracer}]),
               RootSpan1 = passage:set_baggage_items(RootSpan0, #{<<"foo">> => <<"bar">>}),

               ChildSpan0 = passage:start_span(child, [{child_of, RootSpan1}]),
               ?assertEqual(#{<<"foo">> => <<"bar">>},
                            passage:get_baggage_items(ChildSpan0)),

               ChildSpan1 = passage:set_baggage_items(ChildSpan0, #{<<"baz">> => <<"qux">>}),
               ?assertEqual(#{<<"foo">> => <<"bar">>, <<"baz">> => <<"qux">>},
                            passage:get_baggage_items(ChildSpan1))
       end},
      {"baggage item function",
       fun () ->
               ok = start_test_tracer(),
               RootSpan0 = passage:start_span(root, [{tracer, test_tracer}]),
               RootSpan1 = passage:set_baggage_items(
                             RootSpan0, fun () -> #{<<"foo">> => <<"bar">>} end),

               ChildSpan0 = passage:start_span(child, [{child_of, RootSpan1}]),
               ?assertEqual(#{<<"foo">> => <<"bar">>},
                            passage:get_baggage_items(ChildSpan0)),

               ChildSpan1 = passage:set_baggage_items(
                              ChildSpan0, fun () -> #{<<"baz">> => <<"qux">>} end),
               ?assertEqual(#{<<"foo">> => <<"bar">>, <<"baz">> => <<"qux">>},
                            passage:get_baggage_items(ChildSpan1))
       end},
      {"log",
       fun () ->
               ok = start_test_tracer(),
               Span0 = passage:start_span(root, [{tracer, test_tracer}]),
               Span1 = passage:log(Span0, #{hello => world}, [{time, {1, 2, 3}}]),
               Span2 = passage:log(Span1, #{foo => bar}),

               passage:finish_span(Span2),
               [FinishedSpan] = finished_spans(),
               ?assertMatch([{#{foo := bar}, {_, _, _}},
                             {#{hello := world}, {1, 2, 3}}],
                            passage_span:get_logs(FinishedSpan))
       end},
      {"log function",
       fun () ->
               ok = start_test_tracer(),
               Span0 = passage:start_span(root, [{tracer, test_tracer}]),
               Span1 = passage:log(Span0, fun () -> #{hello => world} end,
                                   [{time, {1, 2, 3}}]),
               Span2 = passage:log(Span1, fun () -> #{foo => bar} end),

               passage:finish_span(Span2),
               [FinishedSpan] = finished_spans(),
               ?assertMatch([{#{foo := bar}, {_, _, _}},
                             {#{hello := world}, {1, 2, 3}}],
                            passage_span:get_logs(FinishedSpan))
       end},
      {"error log",
       fun () ->
               ok = start_test_tracer(),
               Span0 = passage:start_span(root, [{tracer, test_tracer}]),
               Span1 = passage:log(Span0, #{message => "Hello World", kind => greeting}, [error]),

               passage:finish_span(Span1),
               [FinishedSpan] = finished_spans(),
               ?assertMatch([{#{event := error, kind := greeting, message := "Hello World"},
                              {_, _, _}}],
                            passage_span:get_logs(FinishedSpan)),
               ?assertEqual(#{error => true},
                            passage_span:get_tags(FinishedSpan))
       end},
      {"strip",
       fun () ->
               ok = start_test_tracer(),
               Root = passage:strip_span(passage:start_span(root, [{tracer, test_tracer}])),

               Span0 = passage:start_span(child, [{child_of, Root}]),
               Span1 = passage:set_tags(Span0, #{a => b}),
               Span2 = passage:log(Span1, #{message => hello}, [{time, 0}]),

               ?assertEqual(#{a => b}, passage_span:get_tags(Span2)),
               ?assertEqual([{#{message => hello}, 0}], passage_span:get_logs(Span2)),
               ?assertEqual([{child_of, Root}], passage_span:get_refs(Span2)),

               Span3 = passage:strip_span(Span2),
               ?assertEqual(#{}, passage_span:get_tags(Span3)),
               ?assertEqual([], passage_span:get_logs(Span3)),
               ?assertEqual([], passage_span:get_refs(Span3))
       end},
      {"lifetime",
       fun () ->
               ok = start_test_tracer(),
               Span = passage:start_span(root, [{tracer, test_tracer}]),
               Pid = spawn(timer, sleep, [infinity]),
               passage:finish_span(Span, [{lifetime, Pid}]),

               ?assertEqual([], finished_spans()),

               exit(Pid, kill),
               monitor(process, Pid),
               receive {'DOWN', _, _, Pid, _} -> ok end,
               timer:sleep(1),

               ?assertMatch([_], finished_spans())
       end},
      {"'sampling.priority' = 0",
       fun () ->
               ok = start_test_tracer(),

               RootSpan =
                   passage:start_span(
                     basic_test,
                     [{tracer, test_tracer}, {tags, #{'sampling.priority' => 0}}]),
               passage:finish_span(RootSpan),
               ?assertEqual([], finished_spans())
       end},
      {"'sampling.priority' = 1",
       fun () ->
               Context = passage_span_context_null,
               Sampler = passage_sampler_null:new(),
               Reporter = passage_reporter_process:new(self(), span),
               ok = passage_tracer_registry:register(
                      test_tracer, Context, Sampler, Reporter),

               RootSpan =
                   passage:start_span(
                     basic_test,
                     [{tracer, test_tracer}, {tags, #{'sampling.priority' => 1}}]),
               passage:finish_span(RootSpan),
               ?assertMatch([_Span], finished_spans())
       end},
      {"unsampled parent",
       fun () ->
               Span0 = passage:start_span(child, [{child_of, undefined}]),
               Span1 = passage:set_operation_name(Span0, foo),
               Span2 = passage:set_tags(Span1, #{111 => 222}),
               Span3 = passage:set_baggage_items(Span2, #{<<"a">> => <<"b">>}),
               Span4 = passage:log(Span3, #{event => debug}),

               passage:finish_span(Span4),
               ?assertEqual([], finished_spans())
       end},
      {"additional references",
       fun () ->
               ok = start_test_tracer(),
               RootSpan = passage:start_span(root, [{tracer, test_tracer}]),
               Span = passage:start_span(
                        child, [{child_of, undefined}, {follows_from, RootSpan}]),

               passage:finish_span(Span),
               ?assertMatch([_Span], finished_spans())
       end},
      {"injection and extraction (noop)",
       fun () ->
               ok = start_test_tracer(),
               Span = passage:start_span(root, [{tracer, test_tracer}]),

               InjectFun = fun (_Key, _Value, Carrier) -> Carrier end,
               ?assertEqual(#{}, passage:inject_span(Span, text_map, InjectFun, #{})),

               IterateFun = fun (_) -> error end,
               ?assertEqual(undefined,
                            passage:extract_span(test_tracer, text_map, IterateFun, #{}))
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
