-module(passage_mailbox).

-behavior(gen_server).

-export([start_link/1]).
-export([post/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(STATE, ?MODULE).

-record(?STATE,
        {
        }).

-spec start_link(passage:tracer()) -> {ok, pid()} | {error, Reason :: term()}.
start_link(Tracer) ->
    gen_server:start_link(passage_local_ns:mailbox_name(Tracer), ?MODULE, [], []).

-spec post(passage_span:span()) -> ok.
post(Span) ->
    Tracer = passage_span:get_tracer(Span),
    gen_server:cast(passage_local_ns:mailbox_name(Tracer), {post, Span}).

init([]) ->
    State = #?STATE{},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({post, Span}, State0) ->
    State1 = handle_post(Span, State0),
    {noreply, State1};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec handle_post(passage_span:span(), #?STATE{}) -> #?STATE{}.
handle_post(Span, State) ->
    error(unimplemented, [Span,State]).
