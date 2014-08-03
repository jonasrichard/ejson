-module(ejson_trans).

-export([
        parse_transform/2
    ]).

-define(D(Val), io:format("~s: ~p~n", [??Val, Val])).

parse_transform(AstIn, _Options) ->
    ?D(AstIn),
    Out = walk(AstIn),
    %%io:format("~p~n", [Out]),
    Out.

walk(Ast) ->
    EofLine = get_eof_line(Ast),
    {AstOut, Records} = walk(Ast, [], []),

    {eof, LastLine} = hd(AstOut),
    
    lists:reverse([{eof, LastLine}, gen_fun(Records, EofLine) | tl(AstOut)]).

walk([], AstOut, Records) ->
    {AstOut, Records};
walk([{attribute, _Line, json, RecordSpec} = Attr | T], A, R) ->
    ?D(RecordSpec),
    walk(T, [Attr | A], [RecordSpec | R]);
walk([H | T], A, R) ->
    walk(T, [H | A], R).

%% Convert json attribute to AST format
field_list([], _Line) ->
    [];
%%field_list([{list, H} | T], Line) ->
%%    {cons, Line,
%%        {tuple, Line, [{atom, Line, list}, {string, Line, H}]},
%%        field_list(T, Line)};
field_list([AtomName | T], Line) when is_atom(AtomName) ->
    [{atom, Line, AtomName} | field_list(T, Line)].

record_to_ast(RecordSpec, Line) ->
    [RecordName | Fields] = tuple_to_list(RecordSpec),
    ?D(Fields),
    {tuple, Line,
        [{atom, Line, RecordName} | field_list(Fields, Line)]}.

record_list_to_ast([], Line) ->
    {nil, Line};
record_list_to_ast([H | T], Line) ->
    {cons, Line, record_to_ast(H, Line), record_list_to_ast(T, Line)}.

gen_fun(Records, Line) ->
    ?D(Records),
    {function, Line, to_json, 1,
        [{clause, Line, [{var, Line, 'P'}], [],
            [{call, Line,
                {remote, Line, {atom, Line, ejson}, {atom, Line, to_json}},
                [{var, Line, 'P'},
                 record_list_to_ast(Records, Line)]}]}]}.

get_eof_line([]) ->
    1;
get_eof_line([{eof, L} | _T]) ->
    L;
get_eof_line([_ | T]) ->
    get_eof_line(T).
