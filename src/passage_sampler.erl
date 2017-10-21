-module(passage_sampler).

-export([new/2]).
-export([is_sampled/4]).

-export_type([sampler/0]).

-callback is_sampled(term(), passage:tracer(), passage:operation_name(), passage:tags()) ->
    boolean().

-opaque sampler() :: {?MODULE, module(), term()}.

-spec is_sampled(sampler(), passage:tracer(), passage:operation_name(), passage:tags()) ->
                        bool.
is_sampled({?MODULE, Module, State}, Tracer, Name, Tags) ->
    Module:is_sampled(State, Tracer, Name, Tags).

-spec new(module(), term()) -> sampler().
new(Module, State) ->
    {?MODULE, Module, State}.
