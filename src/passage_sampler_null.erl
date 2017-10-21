-module(passage_sampler_null).

-behaviour(passage_sampler).

-export([new/0]).

-export([is_sampled/3]).

-spec new() -> passage_sampler:sampler().
new() ->
    passage_sampler:new(?MODULE, undefined).

is_sampled(_State, _OperationName, _Tags) ->
    false.
