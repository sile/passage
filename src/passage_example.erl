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
    _ = hello_child(Name),
    ok.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
-passage_trace([{error_if, "{error, _}"}, error_if_exception]).
-spec hello_child(atom()) -> ok | {error, term()}.
hello_child(Name) ->
    case Name of
        undefined -> {error, unknown_person};
        error     -> error(bad_name);
        _         -> io:format("[child] Hello ~s\n", [Name]), ok
    end.
