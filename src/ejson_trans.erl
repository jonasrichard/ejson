-module(ejson_trans).

-export([
        parse_transform/2,
        format_error/1
    ]).

-define(D(Val), io:format("~s: ~p~n", [??Val, Val])).

parse_transform(AstIn, _Options) ->
    Out = walk(AstIn),
    %%?D(Out),
    Out.

walk(Ast) ->
    EofLine = get_eof_line(Ast),

    %% Collect ejson attributes
    {AstOut, Records, Opts, LastLine} = walk(Ast, [], [], [], 0),

    ToJson = case is_fun_defined(AstOut, to_json, 1) of
                 true ->
                     [];
                 false ->
                     [gen_encode_fun(Records, Opts, EofLine)]
             end,
    FromJson1 = case is_fun_defined(AstOut, from_json, 1) of
                    true ->
                        [];
                    false ->
                        [gen_decode_fun1(Records, Opts, EofLine)]
                end,
    FromJson2 = case is_fun_defined(AstOut, from_json, 2) of
                    true ->
                        [];
                    false ->
                        [gen_decode_fun2(Records, Opts, EofLine)]
                end,

    %% Add function definition if they don't exist yes
    R = lists:reverse([{eof, LastLine}] ++ ToJson ++
                      FromJson1 ++ FromJson2 ++ AstOut),
    R2 = add_compile_options(R),
    %%?D(R2),
    R2.

walk([], AstOut, Records, Opts, LastLine) ->
    {AstOut, Records, Opts, LastLine};
walk([{eof, LastLine} | T], A, R, O, _) ->
    %% Get the number of the last line (we will put from_json and to_json
    %% functions there)
    walk(T, A, R, O, LastLine);
walk([{attribute, _Line, json, RecordSpec} = Attr | T], A, R, O, L) ->
    %% handle -json attribute
    walk(T, [Attr | A], [RecordSpec | R], O, L);
walk([{attribute, Line, json_include, Modules} = Attr | T], A, R, O, L) ->
    %% handle -json_include by reading the records and opts from modules
    Attrs = read_attributes(Line, Modules),
    Opts = read_opts(Line, Modules),
    walk(T, [Attr | A], Attrs ++ R, Opts ++ O, L);
walk([{attribute, _Line, json_opt, Opt} = Attr | T], A, R, O, L) ->
    %% handle -json_opt
    walk(T, [Attr | A], R, [Opt | O], L);
walk([H | T], A, R, O, L) ->
    walk(T, [H | A], R, O, L).

read_attributes(_Line, []) ->
    [];
read_attributes(Line, [Module | Modules]) ->
    case code:load_file(Module) of
        {error, Reason} ->
            report_error(Module, Line, "[ejson] Cannot load module ~p (~p)",
                         [Module, Reason]);
        {module, _} ->
            Js = ejson:json_rules([Module]),
            Js ++ read_attributes(Line, Modules)
    end.

read_opts(_Line, []) ->
    [];
read_opts(Line, [Module | Modules]) ->
    case code:load_file(Module) of
        {error, Reason} ->
            report_error(Module, Line, "[ejson] Cannot load module ~p (~p)",
                         [Module, Reason]);
        {module, _} ->
            Opts = ejson:json_opts([Module]),
            Opts ++ read_opts(Line, Modules)
    end.

report_error(Module, Pos, Msg) ->
    File = atom_to_list(Module) ++ ".erl",
    throw({error,
           [{File, [{Pos, ejson_trans, Msg}]}],
           []}).

report_error(Module, Pos, Format, Args) ->
    report_error(Module, Pos, io_lib:format(Format, Args)).

format_error(Msg) ->
    Msg.

%% true if local function with arity defined
is_fun_defined([], _FunName, _Arity) ->
    false;
is_fun_defined([{function, _Line, FunName, Arity, _} | _Rest], FunName, Arity) ->
    true;
is_fun_defined([_H | Rest], FunName, Arity) ->
    is_fun_defined(Rest, FunName, Arity).

gen_encode_fun(Records, Opts, Line) ->
    {function, Line, to_json, 1,
        [{clause, Line, [{var, Line, 'P'}], [],
            [{call, Line,
                {remote, Line, {atom, Line, ejson}, {atom, Line, to_json}},
                [{var, Line, 'P'},
                 erl_parse:abstract(Records, [{line, Line}]),
                 erl_parse:abstract(Opts, [{line, Line}])]
            }]
        }]
    }.

gen_decode_fun1(Records, Opts, Line) ->
    {function, Line, from_json, 1,
        [{clause, Line, [{var, Line, 'P'}], [],
            [{call, Line,
                {remote, Line, {atom, Line, ejson}, {atom, Line, from_json}},
                [{var, Line, 'P'},
                 erl_parse:abstract(Records, [{line, Line}]),
                 erl_parse:abstract(Opts, [{line, Line}])]
            }]
        }]
    }.
gen_decode_fun2(Records, Opts, Line) ->
    {function, Line, from_json, 2,
        [{clause, Line, [{var, Line, 'P'}, {var, Line, 'Q'}], [],
            [{call, Line,
                {remote, Line, {atom, Line, ejson}, {atom, Line, from_json}},
                [{var, Line, 'P'},
                 {var, Line, 'Q'},
                 erl_parse:abstract(Records, [{line, Line}]),
                 erl_parse:abstract(Opts, [{line, Line}])]
            }]
        }]
    }.

%% Get the line number where the -module(...) attribute is
%% We will put compile attribute there, too.
add_compile_options([]) ->
    [];
add_compile_options([{attribute, Line, module, _Module} = M | R]) ->
    C = {attribute, Line, compile,
         {nowarn_unused_function, [{from_json, 1}, {from_json, 2},
                                   {to_json, 1}]}},
    [M, C | R];
add_compile_options([H | T]) ->
    [H | add_compile_options(T)].

get_eof_line([]) ->
    1;
get_eof_line([{eof, L} | _T]) ->
    L;
get_eof_line([_ | T]) ->
    get_eof_line(T).
