%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Probabilistic Sampler.
%%
%% This sampler samples the spans probabilistically.
%%
%% === Examples ===
%%
%% ```
%% Context = passage_span_context_null,
%% Sampler = passage_sampler_probabilistic:new(0.5),
%% Reporter = passage_reporter_null:new(),
%%
%% ok = passage_tracer_registry:register(foo, Context, Sampler, Reporter).
%% '''
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
%% @doc Makes a new sampler.
%%
%% This will sample spans by the given `SamplingRate'.
%% E.g., if `SamplingRate = 0.5', the half of the spans are sampled probabilistically.
%%
%% The value of `SamplingRate' must be between `0.0' and `1.0'.
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
%% @private
is_sampled(SamplingRate, _OperationName, _Tags) ->
    rand:uniform() < SamplingRate.
