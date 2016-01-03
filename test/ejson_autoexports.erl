-module(ejson_autoexports).

-export([to_json/1]).

-compile({parse_transform, eunit_autoexport}).
-compile({parse_transform, ejson_trans}).

-json({planet,
       {binary, "name"},
       {number, "population", [{default, undefined}]}}).


