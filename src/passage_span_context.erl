%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%%
-module(passage_span_context).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([get_baggage_items/1]).
-export([get_state/1]).

-export_type([context/0]).
-export_type([implementation_module/0]).
-export_type([state/0]).
-export_type([format/0]).
-export_type([carrier/0]).
-export_type([inject_fun/0]).

%%------------------------------------------------------------------------------
%% Application Internal API
%%------------------------------------------------------------------------------
-export([make/2]).
-export([set_baggage_items/2]).

%%------------------------------------------------------------------------------
%% Callback API
%%------------------------------------------------------------------------------
-callback make_span_context_state(passage_span:normalized_refs()) -> state().

-callback inject_span_context(context(), format(), inject_fun(), carrier()) -> carrier().

-callback extract_span_context(format(), iterate_fun(), carrier()) -> {ok, context()} | error.

%%------------------------------------------------------------------------------
%% Macros & Records
%%------------------------------------------------------------------------------
-define(CONTEXT, ?MODULE).

-record(?CONTEXT,
        {
          state               :: state(),
          baggage_items = #{} :: passage:baggage_items()
        }).

%%------------------------------------------------------------------------------
%% Exported Types
%%------------------------------------------------------------------------------
-opaque context() :: #?CONTEXT{}.

-type implementation_module() :: module().

-type state() :: term().

-type format() :: text_map | http_header | binary.

-type carrier() :: term().

-type inject_fun() :: fun ((Key :: binary(), Value :: binary(), carrier()) -> carrier()).

-type iterate_fun() ::
        fun ((carrier()) -> {ok, Key :: binary(), Value :: binary(), carrier()} | error).

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
-spec get_baggage_items(context()) -> passage:baggage_items().
get_baggage_items(Context) ->
    Context#?CONTEXT.baggage_items.

-spec get_state(context()) -> state().
get_state(Context) ->
    Context#?CONTEXT.state.

%%------------------------------------------------------------------------------
%% Application Internal Functions
%%------------------------------------------------------------------------------
%% @private
-spec make(implementation_module(), passage_span:normalized_refs()) -> context().
make(Module, Refs) ->
    State = Module:make_span_context_state(Refs),
    BaggageItems =
        lists:foldl(
          fun ({_, Span}, Acc) ->
                  maps:merge(passage_span:get_baggage_items(Span), Acc)
          end,
          #{},
          Refs),
    #?CONTEXT{state = State, baggage_items = BaggageItems}.

%% @private
-spec set_baggage_items(context(), passage:baggage_items()) -> context().
set_baggage_items(Context, BaggageItems) ->
    Merged = maps:merge(Context#?CONTEXT.baggage_items, BaggageItems),
    Context#?CONTEXT{baggage_items = Merged}.
