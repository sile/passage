%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @private
-module(passage_example).

-compile({parse_transform, passage_transform}).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([hello/1]).

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
-passage_trace([{tags, #{kind => greeting}}, {eval_tags, #{name => "Name"}}]).
-spec hello(atom()) -> ok.
hello(Name) ->
    io:format("Hello ~s\n", [Name]),
    hello_child(Name),
    ok.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
-passage_trace([]).
-spec hello_child(atom()) -> ok.
hello_child(Name) ->
    io:format("[child] Hello ~s\n", [Name]),
    ok.
