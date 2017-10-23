%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Span Context.
%%
%% <blockquote>
%% Each <b>SpanContext</b> encapsulates the following state: <br />
%% <ul>
%%   <li>Any OpenTracing-implementation-dependent state (for example, trace and span ids)
%%       needed to refer to a distinct <b>Span</b> across a process boundary</li>
%%   <li><b>Baggage Items</b>, which are just key:value pairs that
%%       cross process boundaries</li>
%% </ul>
%% <a href="https://github.com/opentracing/specification/blob/1.1/specification.md#the-opentracing-data-model">The OpenTracing Data Model</a>
%% </blockquote>
%%
%% === Callbacks ===
%%
%% This module requires following callbacks:
%%
%% ```
%% %% @doc Creates the state of a span context from the given references.
%% -callback make_span_context_state(passage_span:normalized_refs()) ->
%%     state().
%%
%% %% @doc Injects the span context into the carrier by the specified format.
%% -callback inject_span_context(context(), format(), inject_fun(), carrier()) ->
%%     carrier().
%%
%% %% @doc Extracts a span context from the carrier using the specified format.
%% %%
%% %% If the carrier contains no span context, it will return `error'.
%% -callback extract_span_context(format(), iterate_fun(), carrier()) ->
%%     {ok, context()} | error.
%%
%% '''
-module(passage_span_context).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([make/2]).
-export([get_baggage_items/1]).
-export([get_state/1]).

-export_type([context/0]).
-export_type([implementation_module/0]).
-export_type([state/0]).
-export_type([format/0]).
-export_type([carrier/0]).
-export_type([inject_fun/0]).
-export_type([iterate_fun/0]).

%%------------------------------------------------------------------------------
%% Application Internal API
%%------------------------------------------------------------------------------
-export([from_refs/2]).
-export([set_baggage_items/2]).

%%------------------------------------------------------------------------------
%% Callback API
%%------------------------------------------------------------------------------
-callback make_span_context_state(passage_span:normalized_refs()) -> state().

-callback inject_span_context(context(), format(), inject_fun(), carrier()) -> carrier().

-callback extract_span_context(format(), iterate_fun(), carrier()) ->
    {ok, context()} | error.

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
%% Span context.

-type implementation_module() :: module().
%% Implementation module of this behaviour.

-type state() :: term().
%% Implementation-dependent state.

-type format() :: text_map | http_header | binary.
%% The standard injection/extraction format.
%%
%% <blockquote>
%% Both injection and extraction rely on an extensible <b>format</b> parameter
%% that dictates the type of the associated "carrier" as well as
%% how a `SpanContext' is encoded in that carrier.
%% All of the following <b>format</b>s must be supported by all Tracer implementations.
%% <ul>
%%   <li><b>Text Map</b>: an arbitrary string-to-string map with an unrestricted character set for both keys and values</li>
%%   <li><b>HTTP Headers</b>: a string-to-string map with keys and values that are suitable for use in HTTP headers (a la RFC 7230. In practice, since there is such "diversity" in the way that HTTP headers are treated in the wild, it is strongly recommended that Tracer implementations use a limited HTTP header key space and escape values conservatively.</li>
%%   <li><b>Binary</b>: a (single) arbitrary binary blob representing a `SpanContext'</li>
%% </ul>
%% <a href="https://github.com/opentracing/specification/blob/1.1/specification.md#note-required-formats-for-injection-and-extraction">
%% Note: required formats for injection and extraction
%% (The OpenTracing Semantic Specification)
%% </a>
%% </blockquote>

-type carrier() :: term().
%% Carrier for propagating span contexts.

-type inject_fun() :: fun ((Key :: binary(), Value :: binary(), carrier()) -> carrier()).
%% Span context injection function.
%%
%% If this function is called,
%% the carrier should update own state for injecting `Key' and `Value'.

-type iterate_fun() ::
        fun ((carrier()) -> {ok, Key :: binary(), Value :: binary(), carrier()} | error).
%% Iterator function.
%%
%% If the carrier has any remaining elements,
%% it will return an `ok' tuple that contains a key/value pair and updated state.
%% Otherwise, it will return `error'.

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
%% @doc Makes a new span context.
-spec make(state(), passage:baggage_items()) -> context().
make(State, BaggageItems) ->
    #?CONTEXT{state = State, baggage_items = BaggageItems}.

%% @doc Returns the baggage items of `Context'.
-spec get_baggage_items(context()) -> passage:baggage_items().
get_baggage_items(Context) ->
    Context#?CONTEXT.baggage_items.

%% @doc Returns the state of `Context'.
-spec get_state(context()) -> state().
get_state(Context) ->
    Context#?CONTEXT.state.

%%------------------------------------------------------------------------------
%% Application Internal Functions
%%------------------------------------------------------------------------------
%% @private
-spec from_refs(implementation_module(), passage_span:normalized_refs()) -> context().
from_refs(Module, Refs) ->
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
