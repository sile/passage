%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
-module(passage_reporter_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------
reporter_test_() ->
    [
     {"basic test",
      fun () ->
              Reporter = passage_reporter_null:new(),
              ?assert(passage_reporter:is_reporter(Reporter)),
              ?assertEqual(passage_reporter_null, passage_reporter:get_module(Reporter)),
              ?assertEqual(undefined, passage_reporter:get_state(Reporter))
      end}
    ].
