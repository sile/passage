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
%% -passage_trace([{tags, #{foo => bar}}, {eval_tags, #{size => "byte_size(Bin)"}}]).
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
%%     passage_pd:start_span(foo, [{tags, #{application => example, module => example}}]),
%%     passage_pd:set_tags(#{process => self(), size => byte_size(Bin)}),
%%     <<"foo", Bin/binary>>
%%    after
%%     passage_pd:finish_span()
%%    end.
%% '''
%%
%% === References ===
%%
%% <ul>
%%   <li><a href="erlang.org/doc/apps/erts/absform.html">The Abstract Format</a></li>
%% </ul>
-module(passage_transform).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([parse_transform/2]).

%%------------------------------------------------------------------------------
%% Types & Records
%%------------------------------------------------------------------------------
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
          tags      = #{} :: map(),
          eval_tags = #{} :: map()
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
                  Tags =
                      maps:merge(
                        proplists:get_value(tags, Options, #{}),
                        #{
                           application => State0#state.application,
                           module => State0#state.module
                         }),
                  EvalTags =
                      proplists:get_value(eval_tags, Options, #{}),
                  State1 = State0#state{trace = true, tags = Tags, eval_tags = EvalTags},
                  {State1, Acc};
              ({function, _, Name, _, Clauses} = Form, {State0 = #state{trace = true}, Acc}) ->
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
    [case Clause of
         {clause, Line, Args, Guards, Body} ->
             OperationName = {atom, Line, State#state.function},
             StartOptions =
                 erl_parse:abstract(
                   [{tags, State#state.tags}],
                   [{line, Line}]),
             StartSpan =
                 make_call_remote(
                   Line, passage_pd, 'start_span', [OperationName, StartOptions]),

             EvalTags =
                 {map, Line,
                  [
                   {map_field_assoc, Line,
                    {atom, Line, 'process'}, make_call_remote(Line, erlang, self, [])} |
                   [begin
                        case parse_expr_string(Line, ValueExprStr) of
                            {error, Reason} ->
                                error({bad_passage_eval_tag,
                                       [{module, State#state.module},
                                        {function, State#state.function},
                                        {key, Key}, {value_expr, ValueExprStr},
                                        {error, Reason}]});
                            {ok, Exprs} ->
                                {map_field_assoc, Line, {atom, Line, Key}, {block, Line, Exprs}}
                        end
                    end || {Key, ValueExprStr} <- maps:to_list(State#state.eval_tags)]
                  ]},
             SetTags =
                 make_call_remote(Line, passage_pd, 'set_tags', [EvalTags]),

             FinishSpan =
                 make_call_remote(Line, passage_pd, 'finish_span', []),
             {clause, Line, Args, Guards,
              [
               {'try', Line, [StartSpan, SetTags | Body], [], [], [FinishSpan]}
              ]};
         _ ->
             Clause
     end || Clause <- Clauses].

-spec parse_expr_string(line(), string()) -> {ok, list()} | {error, term()}.
parse_expr_string(Line, ExprStr) ->
    case erl_scan:string(ExprStr ++ ".") of
        {error, Reason, _} -> {error, {cannot_tokenize, Reason}};
        {ok, Tokens0, _}   ->
            Tokens1 = lists:map(fun (T) -> setelement(2, T, Line) end, Tokens0),
            case erl_parse:parse_exprs(Tokens1) of
                {error, Reason} -> {error, {cannot_parse, Reason}};
                {ok, Exprs}     -> {ok, Exprs}
            end
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
