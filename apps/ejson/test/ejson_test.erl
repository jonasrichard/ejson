-module(ejson_test).

-record(person, {
        name,
        birth_year,
        projects}).

-json(person, ["name", "yearOfBirth", {list, "projects"}]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

test() ->
    Person = #person{name = "Jim", birth_year = 1964, projects = []},

    io:format("~s~n", [to_json(Person)]).

-endif.

