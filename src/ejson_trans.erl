-module(ejson_trans).

-export([
        parse_transform/2
    ]).

parse_transform(AstIn, _Options) ->
    %%io:format("~p~n", [AstIn]),
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
walk([{attribute, _Line, json, {RecName, Fields}} = Attr | T], A, R) ->
    walk(T, [Attr | A], [{RecName, Fields} | R]);
walk([H | T], A, R) ->
    walk(T, [H | A], R).

%% Convert json attribute to AST format
field_list([], Line) ->
    {nil, Line};
field_list([{list, H} | T], Line) ->
    {cons, Line,
        {tuple, Line, [{atom, Line, list}, {string, Line, H}]},
        field_list(T, Line)};
field_list([H|T], Line) ->
    {cons, Line, {string, Line, H}, field_list(T, Line)}.

record_to_ast({RecordName, Fields}, Line) ->
    {tuple, Line,
        [{atom, Line, RecordName},
         field_list(Fields, Line)
        ]}.

record_list_to_ast([], Line) ->
    {nil, Line};
record_list_to_ast([H | T], Line) ->
    {cons, Line, record_to_ast(H, Line), record_list_to_ast(T, Line)}.

gen_fun(Records, Line) ->
    {function, Line, to_json, 1,
        [{clause, Line, [{var, Line, 'P'}], [],
            [{call, Line,
                {remote, Line, {atom, Line, json}, {atom, Line, to_json}},
                [{var, Line, 'P'},
                 record_list_to_ast(Records, Line)]}]}]}.

get_eof_line([]) ->
    1;
get_eof_line([{eof, L} | _T]) ->
    L;
get_eof_line([_ | T]) ->
    get_eof_line(T).
