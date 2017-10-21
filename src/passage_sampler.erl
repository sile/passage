-module(passage_sampler).

-export([new/2]).
-export([is_sampled/3]).

-export_type([sampler/0]).

-callback is_sampled(term(), passage:operation_name(), passage:tags()) ->
    boolean().

-opaque sampler() :: {?MODULE, module(), term()}.

-spec is_sampled(sampler(), passage:operation_name(), passage:tags()) -> boolean().
is_sampled({?MODULE, Module, State}, Name, Tags) ->
    Module:is_sampled(State, Name, Tags).

-spec new(module(), term()) -> sampler().
new(Module, State) ->
    {?MODULE, Module, State}.
