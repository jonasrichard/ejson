-module(ejson_trans).

-export([
        parse_transform/2
    ]).

-define(D(Val), io:format("~s: ~p~n", [??Val, Val])).

parse_transform(AstIn, _Options) ->
    Out = walk(AstIn),
    Out.

walk(Ast) ->
    EofLine = get_eof_line(Ast),
    {AstOut, Records} = walk(Ast, [], []),

    {eof, LastLine} = hd(AstOut),
    
    lists:reverse([{eof, LastLine},
                   gen_fun(to_json, Records, EofLine),
                   gen_fun(from_json, Records, EofLine)
                   | tl(AstOut)]).

walk([], AstOut, Records) ->
    {AstOut, Records};
walk([{attribute, _Line, json, RecordSpec} = Attr | T], A, R) ->
    walk(T, [Attr | A], [RecordSpec | R]);
walk([H | T], A, R) ->
    walk(T, [H | A], R).

gen_fun(Name, Records, Line) ->
    {function, Line, Name, 1,
        [{clause, Line, [{var, Line, 'P'}], [],
            [{call, Line,
                {remote, Line, {atom, Line, ejson}, {atom, Line, Name}},
                [{var, Line, 'P'},
                 erl_parse:abstract(Records, [{line, Line}])]
            }]
        }]
    }.

get_eof_line([]) ->
    1;
get_eof_line([{eof, L} | _T]) ->
    L;
get_eof_line([_ | T]) ->
    get_eof_line(T).
