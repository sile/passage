%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
-module(passage_span_context_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------
span_context_test_() ->
    [
     {"basic test",
      fun () ->
              Context0 = passage_span_context:from_refs(passage_span_context_null, []),
              ?assertEqual(undefined, passage_span_context:get_state(Context0)),
              ?assertEqual(#{}, passage_span_context:get_baggage_items(Context0)),

              Context1 =
                  passage_span_context:set_baggage_items(Context0, #{<<"foo">> => <<"bar">>}),
              ?assertEqual(#{<<"foo">> => <<"bar">>},
                           passage_span_context:get_baggage_items(Context1))
      end}
    ].
