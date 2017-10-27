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
    {ok, Child} = hello_child(Name),
    _ = hello_sibling(Child, Name),
    ok.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
-passage_trace([{error_if, "{error, _}"}, error_if_exception]).
-spec hello_child(atom()) -> {ok, passage:maybe_span()} | {error, term()}.
hello_child(Name) ->
    case Name of
        undefined -> {error, unknown_person};
        error     -> error(bad_name);
        _         ->
            io:format("[child] Hello ~s\n", [Name]),
            {ok, passage_pd:current_span()}
    end.

-passage_trace([{follows_from, "Span"}]).
-spec hello_sibling(passage:maybe_span(), atom()) -> ok.
hello_sibling(Span, Name) ->
    io:format("[sibling] Hello ~s\n", [Name]),
    ok.
