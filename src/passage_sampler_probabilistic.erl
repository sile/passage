%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%%
-module(passage_sampler_probabilistic).

-behaviour(passage_sampler).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([new/1]).

%%------------------------------------------------------------------------------
%% 'passage_sampler' Callback API
%%------------------------------------------------------------------------------
-export([is_sampled/3]).

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
-spec new(float()) -> passage_sampler:sampler().
new(SamplingRate) ->
    N = SamplingRate,
    case is_number(N) andalso 0 =< N andalso N =< 1 of
        false -> error(badarg, [SamplingRate]);
        true  -> passage_sampler:new(?MODULE, SamplingRate)
    end.

%%------------------------------------------------------------------------------
%% 'passage_sampler' Callback Functions
%%------------------------------------------------------------------------------
is_sampled(SamplingRate, _OperationName, _Tags) ->
    rand:uniform() < SamplingRate.
