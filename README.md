ejson
=====

JSON library for Erlang on top of jsx

Usage
-----

In order for ejson to take effect the source files need to be compiled with parse_transform ejson_trans. All record which has -json attribute will be converted to JSON.

```erlang
-module(people).
-compile({parse_transform, ejson_trans}).    %% You can specify in rebar.config, too.

-record(person, {name, birth_year, projects}).
-record(project, {name, budget, successful}).

-json({person, ["name", "yearOfBirth", {list, "projects"}]}).
-json({project, ["name", "budget", "isSuccessful"]}).

%% parse_transform generates a to_json/1 local function

start() ->
    Jim = #person{
        name = "Jim",
        birth_year = 1967,
        projects = []},
        
    io:format("~s~n", [to_json(Jim)]).
```

