-module(ejson_trans).

-export([
        parse_transform/2
    ]).

-define(D(Val), io:format("~s: ~p~n", [??Val, Val])).

parse_transform(AstIn, _Options) ->
    Out = walk(AstIn),
    %%?D(Out),
    Out.

walk(Ast) ->
    EofLine = get_eof_line(Ast),

    %% Collect ejson attributes
    {AstOut, Records} = walk(Ast, [], []),

    %% Get the number of the last line (we will put from_json and to_json
    %% functions there)
    {eof, LastLine} = hd(AstOut),
    
    ToJson = case is_fun_defined(AstOut, to_json, 1) of
                 true ->
                     [];
                 false ->
                     [gen_fun(to_json, Records, EofLine)]
             end,
    FromJson = case is_fun_defined(AstOut, from_json, 1) of
                   true ->
                       [];
                   false ->
                       [gen_fun(from_json, Records, EofLine)]
               end,

    %% Add function definition if they don't exist yes
    R = lists:reverse([{eof, LastLine}] ++ ToJson ++ FromJson ++ tl(AstOut)),
    R2 = add_compile_options(R),
    %%?D(R2),
    R2.

walk([], AstOut, Records) ->
    {AstOut, Records};
walk([{attribute, _Line, json, RecordSpec} = Attr | T], A, R) ->
    walk(T, [Attr | A], [RecordSpec | R]);
walk([H | T], A, R) ->
    walk(T, [H | A], R).

%% true if local function with arity defined
is_fun_defined([], _FunName, _Arity) ->
    false;
is_fun_defined([{function, _Line, FunName, Arity, _} | _Rest], FunName, Arity) ->
    true;
is_fun_defined([_H | Rest], FunName, Arity) ->
    is_fun_defined(Rest, FunName, Arity).

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

%% Get the line number where the -module(...) attribute is
%% We will put compile attribute there, too.
add_compile_options([]) ->
    [];
add_compile_options([{attribute, Line, module, _Module} = M | R]) ->
    C = {attribute, Line, compile,
         {nowarn_unused_function, [{from_json, 1}, {to_json, 1}]}},
    [M, C | R];
add_compile_options([H | T]) ->
    [H | add_compile_options(T)].

get_eof_line([]) ->
    1;
get_eof_line([{eof, L} | _T]) ->
    L;
get_eof_line([_ | T]) ->
    get_eof_line(T).
