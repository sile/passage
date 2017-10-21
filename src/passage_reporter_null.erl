-module(passage_reporter_null).

-behaviour(passage_reporter).

-export([new/0]).
-export([report/2]).

-spec new() -> passage_reporter:reporter().
new() ->
    passage_reporter:new(?MODULE, undefined).

report(_, _) ->
    ok.
