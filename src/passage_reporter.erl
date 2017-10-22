%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%%
-module(passage_reporter).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([new/2]).
-export([is_reporter/1]).
-export([get_module/1]).
-export([get_state/1]).

-export_type([reporter/0]).
-export_type([state/0]).

%%------------------------------------------------------------------------------
%% Application Internal API
%%------------------------------------------------------------------------------
-export([report/2]).

%%------------------------------------------------------------------------------
%% Callback API
%%------------------------------------------------------------------------------
-callback report(state(), passage_span:span()) -> Ignored :: term().

%%------------------------------------------------------------------------------
%% Macros & Records
%%------------------------------------------------------------------------------
-define(REPORTER, ?MODULE).

-record(?REPORTER,
        {
          module :: module(),
          state  :: state()
        }).

%%------------------------------------------------------------------------------
%% Exported Types
%%------------------------------------------------------------------------------
-opaque reporter() :: #?REPORTER{}.

-type state() :: term().

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
-spec new(module(), state()) -> passage_reporter:reporter().
new(Module, State) ->
    #?REPORTER{module = Module, state = State}.

-spec is_reporter(reporter() | term()) -> boolean().
is_reporter(X) ->
    is_record(X, ?REPORTER).

-spec get_module(reporter()) -> module().
get_module(Reporter) ->
    Reporter#?REPORTER.module.

-spec get_state(reporter()) -> state().
get_state(Reporter) ->
    Reporter#?REPORTER.state.

%%------------------------------------------------------------------------------
%% Application Internal Functions
%%------------------------------------------------------------------------------
%% @private
-spec report(reporter(), passage_span:span()) -> ok.
report(#?REPORTER{module = Module, state = State}, Span) ->
    Module:report(State, Span),
    ok.
