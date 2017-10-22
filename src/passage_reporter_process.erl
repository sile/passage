%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%%
-module(passage_reporter_process).

-behaviour(passage_reporter).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([new/1]).

%%------------------------------------------------------------------------------
%% 'passage_reporter' Callback API
%%------------------------------------------------------------------------------
-export([report/2]).

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
-spec new(pid()) -> passage_reporter:reporter().
new(DestinationPid) ->
    passage_reporter:new(?MODULE, DestinationPid).

%%------------------------------------------------------------------------------
%% 'passage_reporter' Callback Functions
%%------------------------------------------------------------------------------
report(DestinationPid, Span) ->
    DestinationPid ! Span.
