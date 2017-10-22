%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%%
-module(passage).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([start_root_span/2, start_root_span/3]).
-export([start_span/2, start_span/3]).
-export([finish_span/1, finish_span/2]).
-export([set_operation_name/2]).
-export([set_tags/2]).
-export([set_baggage_items/2]).
-export([get_baggage_items/1]).
-export([log/2, log/3]).
-export([error_log/2, error_log/3, error_log/4, error_log/5]).
-export([inject_span/4, extract_span/4]).

-export_type([tracer_id/0]).
-export_type([maybe_span/0]).
-export_type([operation_name/0]).
-export_type([start_root_span_option/0, start_root_span_options/0]).
-export_type([start_span_option/0, start_span_options/0]).
-export_type([finish_span_option/0, finish_span_options/0]).
-export_type([tags/0, tag_name/0, tag_value/0]).
-export_type([refs/0, ref/0, ref_type/0]).
-export_type([baggage_items/0, baggage_item_name/0, baggage_item_value/0]).
-export_type([log_fields/0, log_field_name/0, log_field_value/0]).
-export_type([log_option/0, log_options/0]).

%%------------------------------------------------------------------------------
%% Exported Types
%%------------------------------------------------------------------------------
-type tracer_id() :: atom().

-type maybe_span() :: passage_span:span() | undefined.

-type operation_name() :: atom().

-type start_root_span_options() :: [start_root_span_option()].

-type start_root_span_option() :: {time, erlang:timestamp()}
                                | {tags, tags()}.

-type start_span_options() :: [start_span_option()].

-type start_span_option() :: {refs, refs()}
                           | start_root_span_option().

-type finish_span_options() :: [finish_span_option()].
-type finish_span_option() :: {time, erlang:timestamp()}.

-type refs() :: [ref()].
-type ref() :: {ref_type(), maybe_span()}.
-type ref_type() :: child_of | follows_from.

-type tags() :: #{tag_name() => tag_value()}.
-type tag_name() :: atom().
-type tag_value() :: term().

-type baggage_items() :: #{baggage_item_name() => baggage_item_value()}.
-type baggage_item_name() :: binary().
-type baggage_item_value() :: binary().

-type log_fields() :: #{log_field_name() => log_field_value()}.
-type log_field_name() :: atom().
-type log_field_value() :: term().

-type log_options() :: [log_option()].

-type log_option() :: {time, erlang:timestamp()}.

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
-spec start_root_span(tracer_id(), operation_name()) -> maybe_span().
start_root_span(Tracer, OperationName) ->
    start_root_span(Tracer, OperationName, []).

-spec start_root_span(tracer_id(), operation_name(), start_root_span_options()) ->
                             maybe_span().
start_root_span(Tracer, OperationName, Options) ->
    passage_span:start_root(Tracer, OperationName, Options).

-spec start_span(operation_name(), ref()) -> maybe_span().
start_span(OperationName, PrimaryReference) ->
    start_span(OperationName, PrimaryReference, []).

-spec start_span(operation_name(), ref(), start_span_options()) -> maybe_span().
start_span(OperationName, PrimaryReference, Options0) ->
    {Refs1, Options2} =
        case lists:keytake(refs, 1, Options0) of
            false                            -> {[], Options0};
            {value, {refs, Refs0}, Options1} -> {Refs0, Options1}
        end,
    Options3 = [{refs, [PrimaryReference | Refs1]} | Options2],
    passage_span:start(OperationName, Options3).

-spec finish_span(maybe_span()) -> ok.
finish_span(Span) ->
    finish_span(Span, []).

-spec finish_span(maybe_span(), finish_span_options()) -> ok.
finish_span(undefined, _)  -> ok;
finish_span(Span, Options) ->
    passage_span:finish(Span, Options).

-spec set_operation_name(maybe_span(), operation_name()) -> maybe_span().
set_operation_name(undefined, _) -> undefined;
set_operation_name(Span, Name)   -> passage_span:set_operation_name(Span, Name).

-spec set_tags(maybe_span(), tags()) -> maybe_span().
set_tags(undefined, _) -> undefined;
set_tags(Span, Tags)   -> passage_span:set_tags(Span, Tags).

-spec set_baggage_items(maybe_span(), baggage_items()) -> ok.
set_baggage_items(undefined, _) -> undefined;
set_baggage_items(Span, Items)  -> passage_span:set_baggage_items(Span, Items).

-spec get_baggage_items(maybe_span()) -> baggage_items().
get_baggage_items(undefined) -> #{};
get_baggage_items(Span)      -> passage_span:get_baggage_items(Span).

-spec log(maybe_span(), log_fields()) -> maybe_span().
log(Span, Fields) ->
    log(Span, Fields, []).

-spec log(maybe_span(), log_fields(), log_options()) -> maybe_span().
log(undefined, _, _)       -> undefined;
log(Span, Fields, Options) -> passage_span:log(Span, Fields, Options).

-spec error_log(maybe_span(), iodata()) -> maybe_span().
error_log(Span, Message) ->
    error_log(Span, Message, []).

-spec error_log(maybe_span(), io:format(), [term()]) -> maybe_span().
error_log(Span, Format, Data) ->
    error_log(Span, Format, Data, #{}).

-spec error_log(maybe_span(), io:format(), [term()], log_fields()) -> maybe_span().
error_log(Span, Format, Data, Fields) ->
    error_log(Span, Format, Data, Fields, []).

-spec error_log(maybe_span(), io:format(), [term()], log_fields(), log_options()) ->
                       maybe_span().
error_log(undefined, _, _, _, _)               -> undefined;
error_log(Span0, Format, Data, Fields0, Options) ->
    Message = io_lib:format(Format, Data),
    Fields1 = maps:merge(Fields0, #{event => error, message => Message}),
    Span1 = passage_span:log(Span0, Fields1, Options),
    passage_span:set_tags(#{error => true}, Span1).

-spec inject_span(Span, Format, InjectFun, Carrier) -> Carrier when
      Span :: maybe_span(),
      Format :: passage_span_context:format(),
      InjectFun :: passage_span_context:inject_fun(),
      Carrier :: passage_span_context:carrier().
inject_span(undefined, _, _, Carrier)         -> Carrier;
inject_span(Span, Format, InjectFun, Carrier) ->
    Context = passage_span:get_context(Span),
    Tracer = passage_span:get_tracer(Span),
    case passage_tracer_registry:get_span_context_module(Tracer) of
        error        -> Carrier;
        {ok, Module} -> Module:inject_span_context(Context, Format, InjectFun, Carrier)
    end.

-spec extract_span(Tracer, Format, IterateFun, Carrier) -> maybe_span() when
      Tracer :: tracer_id(),
      Format :: passage_span_context:format(),
      IterateFun :: passage_span_context:iterate_fun(),
      Carrier :: passage_span_context:carrier().
extract_span(Tracer, Format, IterateFun, Carrier) ->
    case passage_tracer_registry:get_span_context_module(Tracer) of
        error        -> undefined;
        {ok, Module} ->
            case Module:extract_span_context(Format, IterateFun, Carrier) of
                error         -> undefined;
                {ok, Context} -> passage_span:make_extracted_span(Tracer, Context)
            end
    end.
