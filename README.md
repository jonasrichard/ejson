## ejson

JSON library for Erlang on top of `jsx`. It gives a declarative interface for `jsx`, we need to specify conversion rules and `ejson` will convert tuples according to the rules.

### Usage

In order for ejson to take effect the source files need to be compiled with `parse_transform` `ejson_trans`. All record which has `-json` attribute will be converted to JSON.

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

`ejson` extract the first element of the tuples and choose the conversion rule by matching that value to the first element of the rules. A basic rule is an atom name like `person` and a list of JSON attribute names.

#### One to one binding

```erlang
-json({square, ["side"]}).

ejson:to_json({square, 5}).
```

It will be converted as

```json
{
  "side": 5
}
```

#### Lists to arrays

In Erlang lists are strings basically, so if we want to convert list of values we need to specify that.

```erlang
-json({square, ["side"]}).
-json({shapes, [{list, "data"}]}).

ejson:to_json({shapes, [{square, 4}, {square, 6}]}).
```

will be resulted in

```json
{
  "data": [
    {"side": 4},
    {"side": 6}
  ]
}
```

#### Proplists to objects

The keys in proplist will be camel case converted and those will be the name of the attribute in the JSON object.

```erlang
-json({square, ["side"]}).
-json({shapes, [{list, "data"}]}).
-json({canvas, [{proplist, "opts"}]).

ejson:to_json({canvas, [{width, 500}, {height, 300}, {bit_depth: 16}]}).
```

It is a simple object

```json
{
  "width": 500,
  "height": 300,
  "bitDepth": 16
 }
```


#### Transient fields

Sometimes we don't want to dump pids or sockets since they don't make sense on the client side. We can specify `skip` rule to leave them out of the conversion.

```erlang
-json({request, ["path", skip, "method"]}).

ejson:to_json({request, ["/index.html", self(), "GET"]}).
```

Its JSON form is

```json
{
  "path": "/index.html",
  "method": "GET"
}
```

#### Preprocess data (derived fields)

Often we don't want to send `erlang:timestamp()` as is to the client side but we want to calculate a time can be feed into Javascript's `new Date(long)`. We also have possibility to create calculated fields, too. They should be at the very end of the tuple.

```erlang
-module(shapes).

-json({square, ["side", {pre, "area", {shapes, area}}]}).
-json({rect, ["aSide", "bSide", {pre, "area", {shapes, area}]}).
-json({shapes, [{list, "data"}]}).

-export([area/1]).

area({square, A}) -> A * A;
area({rect, A, B}) -> A * B.

ejson:to_json({shapes, [{square, 4}, {rect, 6, 5}]}).
```

We need a function which gets the tuple to be converted as a parameter (`area/1`). In the attribute definition we cannot refer to functions, so we need to write a `{Module, Function}` tuple which refers to `fun Module:Function/1`. Before the conversion that function will be called.

Note: Under the hood `ejson` engine makes pairs from json attributes and tuples element. At first it makes a list of pairs like `[{"side", 4}, {{pre, "area", #Fun}, undefined}]`. Since we want to serialize more fields than we have, the engine will put `undefined`s in the list. That`s why you need to specify the computed attributes at the end of the attribute list.

The result is

```json
{
  "data": [
    {"side": 4, "area": 16},
    {"aSide": 6, "bSide": 5, "area": 30}
  ]
}
```

### Using without parse transform

If one doesn't want to use parse transform for any reason, it is still possible to use ejson but you need to pass the module atom in order that ejson can detect the `-json` attributes.

```erlang
handle_req(Id) ->
    Rec = get_by_id(Id),
    to_json_module(Rec, ?MODULE).
```

### Caching module attributes

Sometimes you don't want to collect the module attributes and filter the json wild attributes for one thousand elements one by one. So there is a possibility to collect them manually and pass them to `to_json/2` function.

```erlang
convert_list(List) ->
    Attrs = ejson:json_props([module1, module2]),
    lists:map(
        fun(Item) ->
            ejson:to_json(Item, Attrs)
        end,
        List).
```
