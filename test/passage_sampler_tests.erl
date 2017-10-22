%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
-module(passage_sampler_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------
sampler_test_() ->
    [
     {"basic test",
      fun () ->
              Sampler = passage_sampler_null:new(),
              ?assert(passage_sampler:is_sampler(Sampler)),
              ?assertEqual(passage_sampler_null, passage_sampler:get_module(Sampler)),
              ?assertEqual(undefined, passage_sampler:get_state(Sampler))
      end}
    ].
