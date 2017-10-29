%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc  A `parse_transform' module for `passage'
%%
%% This module handles `passage_trace' attribute.
%%
%% If the attribute is appeared, the next function will be traced automatically.
%%
%% See following example:
%%
%% ```
%% -module(example).
%%
%% -compile({parse_transform, passage_transform}). % Enables `passage_transform'
%%
%% -passage_trace([{tags, #{foo => "bar", size => "byte_size(Bin)"}}]).
%% -spec foo(binary()) -> binary().
%% foo(Bin) ->
%%   <<"foo", Bin/binary>>.
%% '''
%%
%% The above `foo' function will be transformed as follows:
%%
%% ```
%% foo(Bin) ->
%%   try
%%     passage_pd:start_span('example:foo/1', []),
%%     passage_pd:set_tags(
%%         fun () ->
%%             #{
%%                 'location.pid' => self(),
%%                 'location.application' => example,
%%                 'location.module' => example,
%%                 'location.line' => 7
%%                 foo => bar,
%%                 size => byte_size(Bin)
%%              }
%%         end),
%%     <<"foo", Bin/binary>>
%%   after
%%     passage_pd:finish_span()
%%   end.
%% '''
%%
%% === References ===
%%
%% <ul>
%%   <li><a href="erlang.org/doc/apps/erts/absform.html">The Abstract Format</a></li>
%% </ul>
-module(passage_transform).

-include("opentracing.hrl").

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([parse_transform/2]).

-export_type([passage_trace_option/0]).
-export_type([expr_string/0]).

%%------------------------------------------------------------------------------
%% Exported Types
%%------------------------------------------------------------------------------
-type passage_trace_option() :: {tracer, passage:tracer_id()} |
                                {tags, #{passage:tag_name() => expr_string()}} |
                                {child_of, expr_string()} |
                                {follows_from, expr_string()} |
                                {error_if, expr_string()} |
                                {error_if_exception, boolean()}.
%% <ul>
%% <li><b>tracer</b>: See {@link passage:start_span/2}</li>
%% <li><b>tags</b>: This is the same as {@type passage:tags()} except the values are dynamically evaluated in the transforming phase.</li>
%% <li><b>child_of</b>: See {@link passage:start_span/2}</li>
%% <li><b>follows_from</b>: See {@link passage:start_span/2}</li>
%% <li><b>error_if</b>
%% ```
%% %% {error_if, ErrorPattern}
%% case Body of
%%   ErrorPattern = Error ->
%%     passage_pd:log(#{message, Result}, [error]),
%%     Error;
%%   Ok -> Ok
%% end.
%% '''
%% </li>
%% <li><b>error_if_exception</b>: See {@type passage_pd:with_span_option()}</li>
%% </ul>

-type expr_string() :: string().
%% The textual representation of an expression.
%%
%% When used, it will be converted to an AST representation as follows:
%%
%% ```
%% {ok, Tokens, _} = erl_scan:string(ExprString ++ "."),
%% {ok, [Expr]} = erl_parse:parse_exprs(Tokens).
%% '''

%%------------------------------------------------------------------------------
%% Macros & Types & Records
%%------------------------------------------------------------------------------
-define(MAP_FIELD(Line, K, V),
        {map_field_assoc, Line, {atom, Line, K}, V}).

-define(PAIR(Line, K, V),
        {tuple, Line, [{atom, Line, K}, V]}).

-type form() :: {attribute, line(), atom(), term()}
              | {function, line(), atom(), non_neg_integer(), [clause()]}
              | erl_parse:abstract_form().

-type clause() :: {clause, line(), [term()], [term()], [expr()]}
                | erl_parse:abstract_clause().

-type expr() :: expr_call_remote()
              | expr_var()
              | erl_parse:abstract_expr().

-type expr_call_remote() :: {call, line(), {remote, line(), expr(), expr()}, [expr()]}.
-type expr_var() :: {var, line(), atom()}.

-type line() :: non_neg_integer().

-record(state,
        {
          application :: atom(),
          module      :: module(),
          function    :: atom(),

          trace   = false :: boolean(), % if `true' the next function will be traced

          tracer = error  :: {ok, passage:tracer_id()} | error,
          child_of        :: expr_string() | undefined,
          follows_from    :: expr_string() | undefined,
          tags = #{}      :: #{passage:tag_name() => expr_string()},
          error_if        :: expr_string() | undefined,
          error_if_exception = false :: boolean()
        }).

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
%% @doc Performs transformations for `passage'
-spec parse_transform(AbstractForms, list()) -> AbstractForms when
      AbstractForms :: [term()].
parse_transform(AbstractForms, CompileOptions) ->
    State = #state{
             application = guess_application(AbstractForms, CompileOptions),
             module      = get_module(AbstractForms)
            },
    walk_forms(AbstractForms, State).

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
-spec walk_forms([form()], #state{}) -> [form()].
walk_forms(Forms, State) ->
    {_, ResultFroms} =
        lists:foldl(
          fun ({attribute, _, passage_trace, Options}, {State0, Acc}) ->
                  Tracer =
                      case lists:keyfind(tracer, 1, Options) of
                          false   -> error;
                          {_, Id} -> {ok, Id}
                      end,
                  State1 =
                      State0#state{
                        trace = true,
                        tracer = Tracer,
                        child_of = proplists:get_value(child_of, Options),
                        follows_from = proplists:get_value(follows_from, Options),
                        tags = proplists:get_value(tags, Options, #{}),
                        error_if = proplists:get_value(error_if, Options),
                        error_if_exception =
                            proplists:get_value(error_if_exception, Options, true)
                       },
                  {State1, Acc};
              ({function, _, Name, _,Clauses} = Form, {State0 = #state{trace = true}, Acc}) ->
                  State1 = State0#state{function = Name},
                  NewForm = setelement(5, Form, walk_clauses(Clauses, State1)),
                  {State1#state{trace = false}, [NewForm | Acc]};
              (Form, {State0, Acc}) ->
                  {State0, [Form | Acc]}
          end,
          {State, []},
          Forms),
    lists:reverse(ResultFroms).

-spec walk_clauses([clause()], #state{}) -> [clause()].
walk_clauses(Clauses, State) ->
    [walk_clause(Clause, State) || Clause <- Clauses].

-spec walk_clause(clause(), #state{}) -> clause().
walk_clause({clause, Line, Args, Guards, Body0}, State) ->
    Mfa = io_lib:format("~s:~s/~p",
                        [State#state.module, State#state.function, length(Args)]),
    OperationName = {atom, Line, binary_to_atom(list_to_binary(Mfa), utf8)},
    StartOptions0 =
        case State#state.tracer of
            error        -> erl_parse:abstract([]);
            {ok, Tracer} -> erl_parse:abstract([{tracer, Tracer}])
        end,
    StartOptions1 =
        case State#state.child_of of
            undefined -> StartOptions0;
            ChildOf   ->
                {cons, Line,
                 ?PAIR(Line, child_of, parse_expr_string(Line, ChildOf, State)),
                 StartOptions0}
        end,
    StartOptions2 =
        case State#state.follows_from of
            undefined   -> StartOptions1;
            FollowsFrom ->
                {cons, Line,
                 ?PAIR(Line, follows_from,
                       parse_expr_string(Line, FollowsFrom, State)),
                 StartOptions1}
        end,
    StartSpan =
        make_call_remote(
          Line, passage_pd, 'start_span', [OperationName, StartOptions2]),

    Tags =
        {map, Line,
         [
          ?MAP_FIELD(Line, 'location.pid',
                     make_call_remote(Line, erlang, self, [])),
          ?MAP_FIELD(Line, 'location.application',
                     {atom, Line, State#state.application}),
          ?MAP_FIELD(Line, 'location.module', {atom, Line, State#state.module}),
          ?MAP_FIELD(Line, 'location.line', {integer, Line, Line}) |
          [begin
               ?MAP_FIELD(Line, Key, parse_expr_string(Line, ValueExprStr, State))
           end || {Key, ValueExprStr} <- maps:to_list(State#state.tags)]
         ]},
    SetTags =
        make_call_remote(Line, passage_pd, 'set_tags', [make_fun(Line, [Tags])]),
    Body1 =
        case State#state.error_if of
            undefined  -> Body0;
            ErrorIf ->
                Pattern = parse_expr_string(Line, ErrorIf, State),
                TempVar = make_var(Line, "__Temp"),
                [{match, Line, TempVar, {block, Line, Body0}},
                 {'case', Line, TempVar,
                  [
                   {clause, Line, [Pattern], [],
                    [
                     make_call_remote(
                       Line, passage_pd, log,
                       [
                        {map, Line, [?MAP_FIELD(Line, ?LOG_FIELD_MESSAGE, TempVar)]},
                        {cons, Line, {atom, Line, error}, {nil, Line}}
                       ]),
                     TempVar
                    ]},
                   {clause, Line, [{var, Line, '_'}], [], [TempVar]}
                  ]}]
        end,
    FinishSpan =
        make_call_remote(Line, passage_pd, 'finish_span', []),

    Catch =
        case State#state.error_if_exception of
            false -> [];
            true  ->
                ClassVar = make_var(Line, "__Class"),
                ErrorVar = make_var(Line, "__Error"),
                [
                 {clause, Line,
                  [{tuple, Line, [ClassVar, ErrorVar, {var, Line, '_'}]}],
                  [],
                  [
                   make_call_remote(
                     Line, passage_pd, log,
                     [
                      {map, Line,
                       [
                        ?MAP_FIELD(Line, ?LOG_FIELD_MESSAGE, ErrorVar),
                        ?MAP_FIELD(Line, ?LOG_FIELD_ERROR_KIND, ClassVar),
                        ?MAP_FIELD(
                           Line, ?LOG_FIELD_STACK,
                           make_call_remote(Line, erlang, get_stacktrace, []))

                       ]},
                      {cons, Line, {atom, Line, error}, {nil, Line}}
                     ]),
                   make_call_remote(
                     Line, erlang, raise,
                     [ClassVar, ErrorVar,
                      make_call_remote(Line, erlang, get_stacktrace, [])])
                  ]
                 }
                ]
        end,
    {clause, Line, Args, Guards,
     [
      {'try', Line, [StartSpan, SetTags | Body1], [], Catch, [FinishSpan]}
     ]};
walk_clause(Clause, _State) ->
    Clause.

-spec parse_expr_string(line(), string(), #state{}) -> expr().
parse_expr_string(Line, ExprStr, State) ->
    Result =
        case erl_scan:string(ExprStr ++ ".") of
            {error, Reason, _} -> {error, {cannot_tokenize, Reason}};
            {ok, Tokens0, _}   ->
                Tokens1 = lists:map(fun (T) -> setelement(2, T, Line) end, Tokens0),
                case erl_parse:parse_exprs(Tokens1) of
                    {error, Reason} -> {error, {cannot_parse, Reason}};
                    {ok, [Expr]}    -> {ok, Expr};
                    {ok, Exprs}     -> {error, {must_be_single_expr, Exprs}}
                end
        end,
    case Result of
        {ok, Expression}     -> Expression;
        {error, ErrorReason} ->
            error({eval_failed,
                   [{module, State#state.module},
                    {function, State#state.function},
                    {line, Line},
                    {expr, ExprStr},
                    {error, ErrorReason}]})
    end.

-spec get_module([form()]) -> module().
get_module([{attribute, _, module, Module} | _]) -> Module;  % The `module' attribute will always exist
get_module([_                              | T]) -> get_module(T).

-spec guess_application([form()], list()) -> atom() | undefined.
guess_application(Forms, CompileOptions) ->
    OutDir = proplists:get_value(outdir, CompileOptions),
    SrcDir = case hd(Forms) of
                 {attribute, _, file, {FilePath, _}} -> filename:dirname(FilePath);
                 _                                   -> undefined
             end,
    find_app_file([Dir || Dir <- [OutDir, SrcDir], Dir =/= undefined]).

-spec make_call_remote(line(), module(), atom(), [expr()]) -> expr_call_remote().
make_call_remote(Line, Module, Function, ArgsExpr) ->
    {call, Line, {remote, Line, {atom, Line, Module}, {atom, Line, Function}}, ArgsExpr}.

-spec find_app_file([string()]) -> atom() | undefined.
find_app_file([])           -> undefined;
find_app_file([Dir | Dirs]) ->
    case filelib:wildcard(Dir++"/*.{app,app.src}") of
        [File] ->
            case file:consult(File) of
                {ok, [{application, AppName, _}|_]} -> AppName;
                _                                   -> find_app_file(Dirs)
            end;
        _ -> find_app_file(Dirs)
    end.

-spec make_var(line(), string()) -> expr_var().
make_var(Line, Prefix) ->
    Seq =
        case get({?MODULE, seq}) of
            undefined -> 0;
            Seq0      -> Seq0
        end,
    _ = put({?MODULE, seq}, Seq + 1),
    Name =
        list_to_atom(Prefix ++ "_" ++ integer_to_list(Line) ++ "_" ++ integer_to_list(Seq)),
    {var, Line, Name}.

-spec make_fun(line(), [expr()]) -> expr().
make_fun(Line, Body) ->
    {'fun', Line,
     {clauses, [{clause, Line, [], [], Body}]}}.
