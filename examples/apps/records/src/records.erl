-module(records).

-compile({parse_transform, ejson_trans}).
-compile(warnings_as_errors).

-export([
         start/0, sum_hours/1
        ]).

-json({person, {string, "name"}, {list, "subjects"}, "creditPoints"}).
-json({subject,
       {string, "name"}, 
       {proplist, "meta"},
       "credit",
       {rec_fun, "sumHours", {?MODULE, sum_hours}, {?MODULE, undef}}
      }).

start() ->
    S1 = {subject, "Maths", [{lecture, 2}, {lab, 3}, {essay, 2}], 5},
    S2 = {subject, "PE", [optional, {lab, 2}], 2},
    P = {person, "Paul Smith", [S1, S2], 15},
    
    {ok, Json} = to_json(P),

    io:format("~s~n", [Json]).

sum_hours({subject, _, _, _}) ->
    0.
