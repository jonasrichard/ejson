-module(records).

-compile({parse_transform, ejson_trans}).

-export([
         start/0, sum_hours/1
        ]).

-json({person, "name", {list, "subjects"}, "creditPoints"}).
-json({subject,
       "name", 
       {proplist, "meta"},
       "credit",
       {rec_fun, "sumHours", {?MODULE, sum_hours}, {?MODULE, undef}}
      }).

start() ->
    S1 = {subject, "Maths", [{lecture, 2}, {lab, 3}, {essay, 2}], 5},
    S2 = {subject, "PE", [optional, {lab, 2}], 2},
    P = {person, "Paul Smith", [S1, S2], 15},
    
    io:format("~s~n", [to_json(P)]).

sum_hours({person, _Name, _Subjects, _}) ->
    0.
