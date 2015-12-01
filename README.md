## ejson

[![Build Status](https://travis-ci.org/jonasrichard/ejson.svg?branch=master)](https://travis-ci.org/jonasrichard/ejson)

JSON library for Erlang on top of `jsx`. It gives a declarative interface for `jsx` by which we need to specify conversion rules and `ejson` will convert tuples according to the rules.

I made this library to make easy not just the encoding but rather the decoding of JSONs to Erlang records. I also put emphasis on property-based test the encoding/decoding pair, so all features work in both way.

There are API changes from the previous version see the Changelog section at the bottom.

### Usage

In order for ejson to take effect the source files need to be compiled with `parse_transform` `ejson_trans`. All record which has `-json` attribute can be converted to JSON later.

```erlang
-module(people).
%% You can specify in rebar.config, too.
-compile({parse_transform, ejson_trans}).

-record(person, {name, birth_year, projects}).
-record(project, {name, budget, successful}).

-json({person, {string, "name"}, {number, "yearOfBirth"},
               {list, "projects"}]}).
-json({project, {string, "name"}, {number, "budget"},
                {boolean, "isSuccessful"}]}).

%% parse_transform generates to_json/1 and from_json/{1,2} local functions

start() ->
    Jim = #person{
        name = "Jim",
        birth_year = 1967,
        projects = []},
    {ok, Json} = to_json(Jim),
    io:format("~s~n", [Json]),
    {ok, Person} = from_json(Json, person).  %% Specify the rule name here
```

`ejson` extract the first element of the tuples and choose the conversion rule by matching that value to the first element of the rules. A basic rule is an atom name like `person` and a list of JSON attribute names. In case of decoding we need to specify the target type what we expect to be got.

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

In older versions one didn't need to specify types for numeric, boolean and record fields. Later as features came in - like default values and target types - it was necessary to break the interface and change field rules. Now in case of basic conversion type specifier is always expected.

```erlang
-json({square, {number, "side"}}).

case ejson:to_json({square, 5}) of
    {ok, Json} ->
        io:format("~s", [Json]);
    {error, Reason} ->
        io:format("Error during conversion: ~p", [Reason])
end.
```
It will be converted as

```json
{
  "side": 5
}
```

So numbers and booleans can be converted unambiguously. JSON strings can be coverted to atom, string or binary. Either way we need to specify the target type for the sake of decoding.

```erlang
-json({person, {string, name}, {atom, sex}, {number, age}, {binary, id}}).

to_json({person, "John Doe", male, 43, <<"43231-fec112">>}).
```

Note that ejson puts type information into JSON results, so when we pass those JSONs back, ejson will know what type it needs to convert from those JSONs.

#### Nested records

Obviously not just numeric and boolean values can be in a record but records itselfes. If a field value is a tuple, it will be treated as a record.

```erlang
-json({book, {string, "title"}, {record, "author"}, {number, "year"}}).
-json({author, {string, "firstName"},
               {string, "midName", [{default, ""}]},
               {string, "lastName"}}).
```

In the author field you can put an author record value, the converted will convert in as a nested JSON as you expect. Let us note that ejson doesn't know what type of record can be the value of the author field, so it will generate a type meta-data in the json object (`__rec`). If we don't want to generate that meta-data we need to specify the type of the record like: `{record, "author", [{type, author}]}`.

```json
{
    "title": "How to get things done in 24 hours for dummies",
    "author":
        {
            "__rec": "author",
            "firstName": "John",
            "lastName": "Smith"
        },
    "year": 2014
}
```

During decoding, since the midName is not in the object, it won't be undefined in the record but the empty string as the default value specified.

#### Lists to arrays

In Erlang lists are strings basically, so if we want to convert list of values we need to specify that (tell ejson that it is not a string).

```erlang
-json({square, {number, side}}).
-json({shapes, {list, data}}).

to_json({shapes, [{square, 4}, {square, 6}]}).
```

will be converted to

```json
{
  "__rec" : "shapes",
  "data": [
    {"__rec": "square", "side": 4},
    {"__rec": "square", "side": 6}
  ]
}
```

Since we didn't specify the type of elements in the list a `__rec` field is generated for each object.

#### Typed lists

When we want to process JSONs not generated by ejson we don't have `__rec` fields in the values. So ejson won't know what is the target type of that object (record). So we need to specify in the rules how the elements of the list should be treated.

```erlang
-json({shapes, {list, "data", [{type, square}]}}).
```

In the 3rd parameter of the type definition we can write an option list with which we can refine our rule definition.

#### Proplists to objects

In older version there was some proplist support but it was limited and the encoding/decoding wasn't clear. In the new version proplist can be converted by a generic rule, but the conversion functions need to be implemented by the developer (since there is no clear generic algorithm how to convert proplist to json).

#### Transient fields

Sometimes we don't want to dump pids or sockets since they don't make sense on the client side. We can specify `skip` rule to leave them out of the conversion.

```erlang
-json({request, {string, "path"}, skip, {string, "method"}}).

to_json({request, "/index.html", self(), "GET"}).
```

Its JSON form is

```json
{
  "__rec" : "request",
  "path": "/index.html",
  "method": "GET"
}
```

During decoding we have a missing field (the pid which was skipped). In that case the `from_json` function will fill this skipped fields with undefined. If it is not apropritate for us, we can specify a default value in the skip rule.

```erlang
-json({request, {string, "path"}, {skip, [{default, no_pid}]},
                {string, "method"}}).
```

#### Constant fields

Let us suppose that we have an eventing system and we want to add an event type but only to the JSON and this information isn't in the record to be converted. Then we can add a constant field containing the type information.

```erlang
-json({event, {atom, source}, {const, type, "databaseEvent"}}).

to_json({event, 'node1@127.0.0.1'}).
```

#### Derived fields

Often we don't want to send `os:timestamp()` as is to the client side but we want to calculate a time can be feed into Javascript's `new Date(long)`.

We have the possibility to convert values before passing to JSON encoding engine (and also we can convert values after the JSON engine decoded from an incoming JSON). There are two types of manipulations here: converting the value of a field, or create a new field out of the blue.

When we define a field we can provide `pre_encode/2` or `post_decode/1` hooks. Let us see how to work with `os:timestamp/0`.

```erlang
-module(time).

%% Numeric metric with timestamp time value
-json({metric, {number, time, [{pre_encode, {?MODULE, to_int}},
                               {post_decode, {?MODULE, to_ts}}]},
               {number, value}}).

%% Conversion function needs to be exported
-export([to_int/2, to_ts/1]).

%% Callback gets the whole record and the corresponding field value
%% We can decide with which we want to deal with
to_int(_MetricRecord, {Mega, Sec, Micro}) ->
    %% Convert timestamp to javascript time
    (Mega * 1000000 + Sec) * 1000 + (Micro div 1000).

to_ts(Millis) ->
    Micro = (Millis rem 1000) * 1000,
    T = Millis div 1000,
    Sec = T rem 1000000,
    Mega = (T - Sec) div 1000000.

ejson:to_json({metric, os:timestamp(), 12.5}).
```

During encoding the timestamp will be converted to millisecond values, which can be understood by Javascript. Decoding is done in the same fashion.

When we don't want to (or we cannot) convert our tuples into a format which can be understood by ejson engine, we can convert our value into `jsx:term/0` type and vice versa.

```erlang
-json({invoice, {generic, item, [{pre_encode, {?MODULE, item_to_jsx}},
                                 {post_decode, {?MODULE, jsx_to_item}}]}}).

-export([item_to_jsx/2, jsx_to_item/1]).

%% Here we cannot define well specified rules, so our function will create
%% a list of binary name/value pairs, acceptable by jsx library.
%% The pre_encode function get the record and the field value as parameters.
item_to_jsx(_Invoice, Item) ->
    convert_item(Item).

jsx_to_item(Item) ->
    %% Item is [{BinaryFieldName, BinaryValue}, ...] as jsx defines
    extract_item(Item).
```

#### Override type field generation

Sometimes (especially in case of root records of a record hierarchy) it is comfortable if `ejson` generates `__rec` field to some records. If we want to store records in database we won't know which type of records we will read, so it is logical to generate the `__rec` field to some records. In this example we are storing books and CDs both having author. Author records don't necessary need `__rec` field, but books and CDs need to have.

```erlang
%% Generate __rec field for books and cds
-json_otp({type_field, [book, cd]}).
-json({book, {string, title}, {record, writer, [{type, author}]}}).
-json({cd, {string, title}, {record, musician, [{type, author}]}}).
-json({author, {string, name}}).

store(Key, MediaItem) ->
    Json = to_json(MediaItem),
    db:store(Key, Json).

fetch(Key) ->
    {ok, Json} = db:get(Key),
    %% Here we don't know the type of the item in Json
    {ok, MediaItem} = from_json(Json).

...
    store(112, {book, "Cats everywhere", {author, "Joe Williams"}}),
    Item = fetch(112)
```

So with the help of `-json_opt` we can refine the transformation, here we specified which records must generate `__rec` field whatever happens.

#### Include conversion rules

When we have different set of conversion rules in different files, we can inlcude conversion rules from another file (or files).

```erlang
-json_include(db_person).
-json_include(db_invoice).

...
%% here we can refer to those types in the further -json rules
```

Those files have to be compiled first (at least before the file including them).

#### Virtual fields

It is handy when we need to add fields to the json but those fields don't have pair in the record (we want to generate additional/extra fields beside the record fields). It is also useful when we want to control the decoding process (modify the decoded record depending on fields in json).

In this example we want to serialize a session record which has an id and server name. We have an ets table which stores the pid and node of the session. We are sending the server name, but if it is missing from the incoming json we can repair the session by looking up the ets table.

```erlang
-json({session, {binary, id}, {atom, server},
                {virtual, ets_id, [{pre_encode, {?MODULE, id_to_jsx}},
                                   {post_decode, {?MODULE, jsx_to_id}}]}}).

%% We expect to get the record (without the virtual fields since they don't
%% belong to the record).
id_to_jsx({session, _Id, _Server}) ->
    %% Lookup the session key by pid
    [Server] = ets:lookup(session, Id),
    %% we can provide jsx primitive types here
    null.

%% Here we reconstruct the server if it is not there
jsx_to_id([Id, undefined], _EtsId) ->
    [Server] = ets:lookup(session, Id),
    %% Pass the internal record back
    [Id, Server];
jsx_to_id([Id, Server] = Record, _EtsId) ->
    Record.
```

This is very powerful feature and use only if you understand `ejson` very well. The `pre_encode` function gets the record what we want to convert and it provides a `jsx:term()`. The `post_decode` function gets called during decode phase and it gets the record field values (as a list) which is constructed so far.

If we have a 5-field record and we insert the virtual rule after the 3rd field, the `post_decode` function will get a three-long list (!). Because at that phase of the decoding we only have only 3 fields decoded.

### Using without parse transform

If one doesn't want to use parse transform for any reason, it is still possible to use ejson but you need to pass the module atom list in order that ejson can detect the `-json` attributes.

```erlang
handle_req(Id) ->
    Rec = get_by_id(Id),
    ejson:to_json_modules(Rec, [?MODULE]).
```

### Caching module attributes

Sometimes you don't want to collect the module attributes and filter the json wild attributes for one thousand elements one by one. So there is a possibility to collect them manually and pass them to `to_json/2` function.

```erlang
convert_list(List) ->
    Opts = ejson:json_props([module1, module2]),
    lists:map(
        fun(Item) ->
            ejson:to_json(Item, Opts)
        end,
        List).
```

### Use with parse transform

If `ejson_trans` parse transform is used, json attributes are read, and two local functions (`from_json` and `to_json`) are generated to support conversion. If we want to use json rules from another module, we can do that by using `json_include` attribute.

```erlang
-module(book_rest).

-compile({parse_transform, ejson_trans}).

-json_include([book, author]).      %% Use json rules from another modules

-json({book_get_req, {string, id}}).
-json({book_get_resp, {record, book, [{type, book}]}}).

...
```

The modules are loaded by `code:load_file/1`, so depending modules need to be compiled first or at least before that file.

### Changes

ejson is developer organically so new features were added as they were required and also old features were refined. Use the latest version if possible. 

### Changelog from 0.1.x

* Field rule specifications are no longer in a list `-json({record, [field1, field2]})` is simplified to `-json({record, field1, field2})`.
* Converter generate a JSON field describing the source record of the data. The `__rec` field contains the record name, which is useful for decoding.
* Encoded proplists also contain type field `__type` which the decoder know what is the target type.

### Changelog from 0.2.x

* All `to` and `from` functions in `ejson` module result in either an `{ok, Result}` or an `{error, Reason}` term. Previously it was `Result` and `{error, Reason}`.

### Changelog from 0.3.0

* ejson doesn't generate `__rec` field only when it is needed (untyped list, untyped record)
* In case of decoding we always need to specify the target type, so `from_json/2` is generated instead of the 1 arity function.
