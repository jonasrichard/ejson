-module(records).

-compile({parse_transform, ejson_trans}).
-compile(warnings_as_errors).

-export([
         start/0, sum_hours/1
        ]).

-json_opt({type_field, [person]}).

-json({person, {string, "name"},
               {list, "subjects", [{type, subject}]}}).

-json({subject, {string, "name"}, {number, credit}}).

start() ->
    S1 = {subject, "Maths", 3},
    S2 = {subject, "PE", 2},
    P = {person, "Paul Smith", [S1, S2]},
    
    {ok, Json} = to_json(P),

    io:format("~s~n", [Json]).

sum_hours({subject, _, _, _}) ->
    0.
