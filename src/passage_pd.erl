%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%%
-module(passage_pd).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([start_root_span/2, start_root_span/3]).
-export([start_span/1, start_span/2]).
-export([finish_span/0, finish_span/1]).
-export([with_root_span/3, with_root_span/4]).
-export([with_span/2, with_span/3]).
-export([current_span/0]).
-export([set_operation_name/1]).
-export([set_tags/1]).
-export([set_baggage_items/1]).
-export([get_baggage_items/0]).
-export([log/1, log/2]).
-export([error_log/1, error_log/2, error_log/3, error_log/4]).

%%------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------
-define(ANCESTORS_KEY, passage_span_ancestors).

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
-spec start_root_span(passage:operation_name(), passage:tracer_id()) -> ok.
start_root_span(OperationName, Tracer) ->
    start_root_span(OperationName, Tracer, []).

-spec start_root_span(passage:operation_name(), passage:tracer_id(), passge:start_root_span_options()) -> ok.
start_root_span(OperationName, Tracer, Options) ->
    case passage:start_root_span(OperationName, Tracer, Options) of
        undefined -> ok;
        Span      -> put_ancestors([Span | get_ancestors()])
    end.

-spec start_span(passage:operation_name()) -> ok.
start_span(OperationName) ->
    start_span(OperationName, []).

-spec start_span(passage:operation_name(), passage:start_span_options()) -> ok.
start_span(OperationName, Options) ->
    Ancestors = get_ancestors(),
    Options1 =
        case Ancestors of
            []           -> Options;
            [Parent | _] ->
                Refs = proplists:get_value(refs, Options, []),
                [{refs, [{child_of, Parent} | Refs]} | Options]
        end,
    case passage_span:start(OperationName, Options1) of
        undefined -> ok;
        Span      -> put_ancestors([Span | Ancestors])
    end.

-spec finish_span() -> ok.
finish_span() ->
    finish_span([]).

-spec finish_span(passage:finish_span_option()) -> ok.
finish_span(Options) ->
    Span = pop_span(),
    passage:finish_span(Span, Options).

-spec with_root_span(passage:operation_name(), passage:tracer_id(), Fun) -> Result when
      Fun :: fun (() -> Result),
      Result :: term().
with_root_span(OperationName, Tracer, Fun) ->
    with_root_span(OperationName, Tracer, [], Fun).

-spec with_root_span(passage:operation_name(), passage:tracer_id(), passage:start_span_options(), Fun) -> Result when
      Fun :: fun (() -> Result),
      Result :: term().
with_root_span(OperationName, Tracer, Options, Fun) ->
    try
        start_root_span(OperationName, Tracer, Options),
        Fun()
    after
        finish_span()
    end.

-spec with_span(passage:operation_name(), Fun) -> Result when
      Fun :: fun (() -> Result),
      Result :: term().
with_span(OperationName, Fun) ->
    with_span(OperationName, [], Fun).

-spec with_span(passage:operation_name(), passage:start_span_options(), Fun) -> Result when
      Fun :: fun (() -> Result),
      Result :: term().
with_span(OperationName, Options, Fun) ->
    try
        start_span(OperationName, Options),
        Fun()
    after
        finish_span()
    end.

-spec current_span() -> passage:maybe_span().
current_span() ->
    case get(?ANCESTORS_KEY) of
        undefined  -> undefined;
        []         -> undefined;
        [Span | _] -> Span
    end.

-spec set_operation_name(passage:operation_name()) -> ok.
set_operation_name(OperationName) ->
    update_current_span(
      fun (Span) -> passage_span:set_operation_name(Span, OperationName) end).

-spec set_tags(passage:tags()) -> ok.
set_tags(Tags) ->
    update_current_span(fun (Span) -> passage_span:set_tags(Span, Tags) end).

-spec set_baggage_items(passage:baggage_items()) -> ok.
set_baggage_items(Items) ->
    update_current_span(fun (Span) -> passage_span:set_baggage_items(Span, Items) end).

-spec get_baggage_items() -> passage:baggage_items().
get_baggage_items() ->
    Span = current_span(),
    passage:get_baggage_items(Span).

-spec log(passage:log_fields()) -> ok.
log(Fields) ->
    log(Fields, []).

-spec log(passage:log_fields(), passage:log_option()) -> ok.
log(Fields, Options) ->
    update_current_span(fun (Span) -> passage:log(Span, Fields, Options) end).

-spec error_log(iodata()) -> ok.
error_log(Message) ->
    error_log(Message, []).

-spec error_log(io:format(), [term()]) -> ok.
error_log(Format, Data) ->
    error_log(Format, Data, #{}).

-spec error_log(io:format(), [term()], passage:log_fields()) -> ok.
error_log(Format, Data, Fields) ->
    error_log(Format, Data, Fields, []).

-spec error_log(io:format(), [term()], passage:log_fields(), passage:log_options()) -> ok.
error_log(Format, Data, Fields, Options) ->
    update_current_span(
      fun (Span) -> passage:error_log(Span, Format, Data, Fields, Options) end).

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
-spec get_ancestors() -> [passage:maybe_span()].
get_ancestors() ->
    case get(?ANCESTORS_KEY) of
        undefined -> [];
        Ancestors -> Ancestors
    end.

-spec put_ancestors([passage:maybe_span()]) -> ok.
put_ancestors(Ancestors) ->
    put(?ANCESTORS_KEY, Ancestors),
    ok.

-spec update_current_span(Fun) -> ok when
      Fun :: fun ((passage_span:span()) -> passage_span:span()).
update_current_span(Fun) ->
    case get(?ANCESTORS_KEY) of
        undefined       -> ok;
        []              -> ok;
        [Span0 | Spans] ->
            Span1 = Fun(Span0),
            put_ancestors([Span1 | Spans])
    end.

-spec pop_span() -> passage:maybe_span().
pop_span() ->
    case get(?ANCESTORS_KEY) of
        undefined      -> undefined;
        []             -> undefined;
        [Span | Spans] ->
            put_ancestors(Spans),
            Span
    end.
