-module(ejson_no_json_attr).

-compile({parse_transform, ejson_trans}).

-include_lib("eunit/include/eunit.hrl").

no_json_function_defined_test_() ->
    Funs = ?MODULE:module_info(functions),
    {"No ejson funs generated if no rules",
     [?_assertNot(lists:member({to_json, 1}, Funs)),
      ?_assertNot(lists:member({from_json, 1}, Funs)),
      ?_assertNot(lists:member({from_json, 2}, Funs))]}.
