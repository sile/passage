-module(passage_reporter).

-export([new/2]).
-export([report/2]).

-export_type([reporter/0]).

-callback report(passage_span:span(), term()) -> term().

-opaque reporter() :: {?MODULE, module(), term()}.

-spec new(module(), term()) -> passage_reporter:reporter().
new(Module, State) ->
    {?MODULE, Module, State}.

-spec report(reporter(), passage_span:span()) -> ok.
report({?MODULE, Module, State}, Span) ->
    Module:report(Span, State),
    ok.
