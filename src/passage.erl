-module(passage).

-export([start_span/1, start_span/2]).
-export([finish_span/0, finish_span/1]).
-export([with_span/2, with_span/3]).
-export([pop_span/0, push_span/1]).
-export([set_tags/1]).
-export([log/1, log/2]).
-export([error_log/1, error_log/2, error_log/3]).
-export([set_baggage_items/1]).
-export([get_baggage_items/0]).
-export([get_span_context/0]).

-export_type([operation_name/0]).
-export_type([start_span_option/0]).
-export_type([finish_span_option/0]).
-export_type([log_fields/0, log_field_name/0, log_field_value/0]).
-export_type([log_option/0]).
-export_type([error_log_option/0]).
-export_type([tracer_id/0]).
-export_type([span/0]).
-export_type([refs/0, ref/0, ref_type/0]).
-export_type([tags/0, tag_name/0, tag_value/0]).
-export_type([baggage_items/0, baggage_item_name/0, baggage_item_value/0]).

-type operation_name() :: atom().

-type tracer_id() :: atom().

-type start_span_option() :: {tracer, tracer_id()}
                           | {time, erlang:timestamp()}
                           | {refs, refs()}
                           | {tags, tags()}.

-type finish_span_option() :: {time, erlang:timestamp()}.

-type span() :: passage_span:maybe_span().
-type span_context() :: passage_span:maybe_span_context().

-type refs() :: [ref()].
-type ref() :: {ref_type(), span() | span_context()}.
-type ref_type() :: child_of | follows_from.

-type tags() :: #{tag_name() => tag_value()}.
-type tag_name() :: atom().
-type tag_value() :: term().

-type log_fields() :: #{log_field_name() => log_field_value()}.
-type log_field_name() :: atom().
-type log_field_value() :: term().

-type log_option() :: {time, erlang:timestamp()}.

-type error_log_option() :: log_option()
                          | {kind, atom()}
                          | stacktrace
                          | {stacktrace, [erlang:passage_item()]}.

-type baggage_items() :: #{baggage_item_name() => baggage_item_value()}.
-type baggage_item_name() :: binary().
-type baggage_item_value() :: binary().

-define(ANCESTORS_KEY, passage_span_ancestors).

-spec start_span(operation_name()) -> ok.
start_span(OperationName) ->
    start_span(OperationName, []).

-spec start_span(operation_name(), [start_span_option()]) -> ok.
start_span(OperationName, Options) ->
    Ancestors = get_ancestors(),
    Options1 =
        case Ancestors of
            []              -> Options;
            [undefined | _] -> Options;
            [Parent    | _] ->
                Refs = proplists:get_value(refs, Options, []),
                [{refs, [{child_of, Parent} | Refs]} | Options]
        end,
    Span = passage_span:start(OperationName, Options1),
    put_ancestors([Span | Ancestors]).

-spec finish_span() -> ok.
finish_span() ->
    finish_span([]).

-spec finish_span([finish_span_option()]) -> ok.
finish_span(Options) ->
    Span = pop_span(),
    passage_span:finish(Span, Options).

-spec with_span(operation_name(), Fun) -> Result when
      Fun :: fun (() -> Result),
      Result :: term().
with_span(OperationName, Fun) ->
    with_span(OperationName, [], Fun).

-spec with_span(operation_name(), [start_span_option()], Fun) -> Result when
      Fun :: fun (() -> Result),
      Result :: term().
with_span(OperationName, Options, Fun) ->
    try
        start_span(OperationName, Options),
        Fun()
    after
        finish_span()
    end.

-spec set_tags(tags()) -> ok.
set_tags(Tags) ->
    update_current_span(fun (Span) -> passage_span:set_tags(Span, Tags) end).

-spec log(log_fields()) -> ok.
log(Fields) ->
    log(Fields, []).

-spec log(log_fields(), [log_option()]) -> ok.
log(Fields, Options) ->
    update_current_span(fun (Span) -> passage_span:log(Span, Fields, Options) end).

-spec error_log(iodata()) -> ok.
error_log(Message) ->
    error_log(Message, []).

-spec error_log(io:format(), [term()]) -> ok.
error_log(Format, Data) ->
    error_log(Format, Data, []).

-spec error_log(io:format(), [term()], [error_log_option()]) -> ok.
error_log(Format, Data, Options) ->
    update_current_span(
      fun (Span0) ->
              Message = io_lib:format(Format, Data),
              Fields =
                  [
                   {event, error},
                   {message, Message}
                  ] ++
                  proplists:lookup_all(kind, Options) ++
                  case proplists:lookup(stacktrace, Options) of
                      none      -> [];
                      {_, true} ->
                          Stack = try error(dummy) catch _:_ -> erlang:get_stacktrace() end,
                          [{stack, Stack}];
                      {_, Stack} -> [{stack, Stack}]
                  end,
              Span1 = passage_span:log(Span0, maps:from_list(Fields), Options),
              passage_span:set_tags(#{error => true}, Span1)
      end).

-spec set_baggage_items(baggage_items()) -> ok.
set_baggage_items(Items) ->
    update_current_span(fun (Span) -> passage_span:set_baggage_items(Span, Items) end).

-spec get_baggage_items() -> baggage_items().
get_baggage_items() ->
    Span = current_span(),
    passage_span:get_baggage_items(Span).

-spec pop_span() -> span().
pop_span() ->
    case get(?ANCESTORS_KEY) of
        undefined      -> undefined;
        [Span | Spans] ->
            put_ancestors(Spans),
            Span
    end.

-spec push_span(span()) -> ok.
push_span(Span) ->
    put_ancestors([Span | get_ancestors()]).

-spec get_span_context() -> span_context().
get_span_context() ->
    passage_span:get_context(current_span()).

-spec update_current_span(Fun) -> ok when
      Fun :: fun ((passage_span:span()) -> passage_span:span()).
update_current_span(Fun) ->
    case get(?ANCESTORS_KEY) of
        undefined       -> ok;
        [undefined | _] -> ok;
        [Span0 | Spans] ->
            Span1 = Fun(Span0),
            put_ancestors([Span1 | Spans])
    end.

-spec current_span() -> span().
current_span() ->
    case get(?ANCESTORS_KEY) of
        undefined  -> undefined;
        [Span | _] -> Span
    end.

-spec get_ancestors() -> [span()].
get_ancestors() ->
    case get(?ANCESTORS_KEY) of
        undefined -> [];
        Ancestors -> Ancestors
    end.

-spec put_ancestors([span()]) -> ok.
put_ancestors(Ancestors) ->
    put(?ANCESTORS_KEY, Ancestors),
    ok.
