%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Span Reporter.
%%
%% Note that this component has not been described in the
%% <a href="https://github.com/opentracing/specification/blob/1.1/specification.md">OpenTracing Specification</a>.
%%
%% === Callbacks ===
%%
%% This module requires following callback:
%%
%% ```
%% %% @doc Reports the finished span to an external observer (e.g., Jaeger agent).
%% -callback report(state(), FinishedSpan :: passage_span:span()) -> Ignored :: term().
%% '''
-module(passage_reporter).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([new/2]).
-export([is_reporter/1]).
-export([get_module/1]).
-export([get_state/1]).
-export([report/2]).

-export_type([reporter/0]).
-export_type([state/0]).

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
%% Reporter.

-type state() :: term().
%% Implementation-dependent state.

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
%% @doc Makes a new reporter.
-spec new(module(), state()) -> passage_reporter:reporter().
new(Module, State) ->
    #?REPORTER{module = Module, state = State}.

%% @doc Returns `true' if `X' is a reporter, otherwise `false'.
-spec is_reporter(reporter() | term()) -> boolean().
is_reporter(X) ->
    is_record(X, ?REPORTER).

%% @doc Returns the module of `Reporter'.
-spec get_module(reporter()) -> module().
get_module(Reporter) ->
    Reporter#?REPORTER.module.

%% @doc Returns the state of `Reporter'.
-spec get_state(reporter()) -> state().
get_state(Reporter) ->
    Reporter#?REPORTER.state.

%% @doc Reports the given finished span.
-spec report(reporter(), passage_span:span()) -> ok.
report(#?REPORTER{module = Module, state = State}, Span) ->
    Module:report(State, Span),
    ok.
