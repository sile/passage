%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%%
-module(passage_reporter_process).

-behaviour(passage_reporter).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([new/2]).

%%------------------------------------------------------------------------------
%% 'passage_reporter' Callback API
%%------------------------------------------------------------------------------
-export([report/2]).

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
-spec new(pid(), term()) -> passage_reporter:reporter().
new(DestinationPid, Tag) ->
    passage_reporter:new(?MODULE, {DestinationPid, Tag}).

%%------------------------------------------------------------------------------
%% 'passage_reporter' Callback Functions
%%------------------------------------------------------------------------------
report({DestinationPid, Tag}, Span) ->
    DestinationPid ! {Tag, Span}.
