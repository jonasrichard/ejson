## ejson

JSON library for Erlang on top of `jsx`. It gives a declarative interface for `jsx`, we need to specify conversion rules and `ejson` will convert tuples according to the rules.

There are API changes from the previous version see the Changelog section at the bottom.

### Usage

In order for ejson to take effect the source files need to be compiled with `parse_transform` `ejson_trans`. All record which has `-json` attribute will be converted to JSON.

```erlang
-module(people).
%% You can specify in rebar.config, too.
-compile({parse_transform, ejson_trans}).

-record(person, {name, birth_year, projects}).
-record(project, {name, budget, successful}).

-json({person, {string, "name"}, "yearOfBirth", {list, "projects"}]}).
-json({project, {string, "name"}, "budget", "isSuccessful"]}).

%% parse_transform generates to_json/1 and from_json/1 local functions

start() ->
    Jim = #person{
        name = "Jim",
        birth_year = 1967,
        projects = []},
        
    io:format("~s~n", [to_json(Jim)]).
```

`ejson` extract the first element of the tuples and choose the conversion rule by matching that value to the first element of the rules. A basic rule is an atom name like `person` and a list of JSON attribute names.

#### JSON property names

JSON property names come from the rules we define with the `-json` attribute. Names can be string in that case the string will be the property name. Names also can be atoms in that case atoms will be camel cased strings according to the `ejson_util:atom_to_binary_cc/1` function.

```
"name" -> "name"
"firstName" -> "firstName"
side -> "side"
avg_val -> "avgVal"
a_typical_atom_name -> "aTypicalAtomName"
```

#### One to one binding

```erlang
-json({square, "side"}).

ejson:to_json({square, 5}).
```
It will be converted as

```json
{
  "__rec": "square",
  "side": 5
}
```

Let us note that in the attribute definition there is not type specified for side. During the data conversion between Erlang and JSON only numbers and booleans can be converted unambiguously. From a JSON string an Erlang string, binary or atom can be extracted. So if we are dealing with those data types we need to specify the target type (which is really useful when we are decoding from JSON).

```erlang
-json({person, {string, name}, {atom, sex}, age, {binary, id}}).

ejson:to_json({person, "John Doe", male, 43, <<"43231-fec112">>}).
```

#### Lists to arrays

In Erlang lists are strings basically, so if we want to convert list of values we need to specify that.

```erlang
-json({square, side}).
-json({shapes, {list, data}}).

ejson:to_json({shapes, [{square, 4}, {square, 6}]}).
```

will be resulted in

```json
{
  "__rec" : "shapes",
  "data": [
    {"__rec": "square", "side": 4},
    {"__rec": "square", "side": 6}
  ]
}
```

#### Proplists to objects

The keys in proplist will be camel case converted and those will be the name of the attribute in the JSON object. In order that the decoder can create proplist from an object a `__type` property will be added as an extra.

```erlang
-json({square, side}).
-json({shapes, {list, data}}).
-json({canvas, {proplist, opts}).

ejson:to_json({canvas, [{width, 500}, {height, 300}, {bit_depth: 16}]}).
```

It is a simple object

```json
{
  "__type": "proplist",
  "width": 500,
  "height": 300,
  "bitDepth": 16
 }
```

#### Transient fields

Sometimes we don't want to dump pids or sockets since they don't make sense on the client side. We can specify `skip` rule to leave them out of the conversion.

```erlang
-json({request, "path", skip, "method"}).

ejson:to_json({request, ["/index.html", self(), "GET"]}).
```

Its JSON form is

```json
{
  "__rec" : "request",
  "path": "/index.html",
  "method": "GET"
}
```

#### Constant fields

Let us suppose that we have an eventing system and we want to add an event type but only to the JSON and this information isn't in the record to be converted. Then we can add a constant field containing the type information.

```
-json({event, {atom, source}, {const, type, "databaseEvent"}}).

ejons:to_json({event, 'node1@127.0.0.1'}).
```

#### Derived fields

Often we don't want to send `erlang:timestamp()` as is to the client side but we want to calculate a time can be feed into Javascript's `new Date(long)`.

There are two types of functions can be applied: field functions and record functions. Field functions are applied to the current field, record functions are always applied to the whole record.

Record functions can be used only during encoding, it means that we can create values that don't exist in the original record and can be computed from the whole record. But during decoding the values we got from JSON will be dropped for those fields. Field functions however two-way functions. If a time should be converted into Javascript time we need to provide the dual function which will convert Javascript time to - let us say - Erlang timestamp.

```erlang
-module(shapes).

-record({square, {side}}).
-record({rect, {a_side, b_side}}).

-json({square, side, {rec_fun, area, {shapes, area}}}).
-json({rect, ["aSide", "bSide", {rec_fun, "area", {shapes, area}]}).
-json({shapes, [{list, "data"}]}).

%% Conversion function needs to be exported
-export([area/1]).

area({square, A}) -> A * A;
area({rect, A, B}) -> A * B.

ejson:to_json({shapes, [{square, 4}, {rect, 6, 5}]}).
```

We need a function which gets the tuple to be converted as a parameter (`area/1`). In the attribute definition we cannot refer to functions, so we need to write a `{Module, Function}` tuple which refers to `fun Module:Function/1`.

Note: Under the hood `ejson` engine makes pairs from json attributes and tuples element. At first it makes a list of pairs like `[{"side", 4}, {{pre, "area", #Fun}, undefined}]`. Since we want to serialize more fields than we have, the engine will put `undefined`s in the list. That`s why you need to specify the computed attributes at the end of the attribute list.

The result is

```json
{
  "__rec" : "shapes",
  "data": [
    {"__rec": "square", "side": 4, "area": 16},
    {"__rec": "rect", "aSide": 6, "bSide": 5, "area": 30}
  ]
}
```

Another example using `field_fun` to convert times.

```
-json({event, id, {field_fun, time, {?MODULE, to_jstime},
                                    {?MODULE, from_jstime}}}).

-export([to_jstime/1, from_jstime/1]).

to_jstime({Macro, Time, Micro}) ->
    ((Macro * 1000000) + Time) * 1000 + Micro div 1000.

from_jstime(JsTime) ->
    Micro = (JsTime rem 1000) * 1000,
    Time = (JsTime div 1000) rem 100000,
    Macro = JsTime div (1000 * 1000000),
    {Macro, Time, Micro}.

ejson:to_json({event, 300, {{1460, 1329112, 706500}}}).
```

### Using without parse transform

If one doesn't want to use parse transform for any reason, it is still possible to use ejson but you need to pass the module atom list in order that ejson can detect the `-json` attributes.

```erlang
handle_req(Id) ->
    Rec = get_by_id(Id),
    to_json_modules(Rec, [?MODULE]).
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

### Changelog

* Field rule specifications are no longer in a list `-json({record, [field1, field2]})` is simplified to `-json({record, field1, field2})`.
* Converter generate a JSON field describing the source record of the data. The `__rec`` field contains the record name, which is useful for decoding.
* Encoded proplists also contain type field `__type` which the decoder know what is the target type.
