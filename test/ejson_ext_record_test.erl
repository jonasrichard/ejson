-module(ejson_ext_record_test).

-include_lib("eunit/include/eunit.hrl").

-json_include(ejson_ext1).

-compile({parse_transform, ejson_trans}).

-import(ejson_test_util, [json_path/2, assertJsonPathList/2]).

encode_test_() ->
    Rec = {account, "joe@test.com", "password",
            {user, {group, "admin"}}},

    {ok, Json} = to_json(Rec),


    assertJsonPathList(Json,
                       [{"email", <<"joe@test.com">>},
                        {"password", <<"password">>},
                        {"__rec", undefined},
                        {"owner.__rec", undefined},     %% Since owner is typed
                        {"owner.group.__rec", undefined},
                        {"owner.group.name", <<"admin">>}
                       ]).

