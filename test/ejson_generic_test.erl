-module(ejson_generic_test).

-export([to_jsx/2, from_jsx/1,
         dict_to_entries/2, entries_to_dict/1]).

-import(ejson_test_util, [json_prop/2, json_path/2, jsx_prop/2, jsx_path/2]).

-include_lib("eunit/include/eunit.hrl").


to_jsx(_Tuple, {High, Low}) ->
    [{<<"high">>, High}, {<<"low">>, Low}].

from_jsx(Attrs) ->
    {_, High} = lists:keyfind(<<"high">>, 1, Attrs),
    {_, Low} = lists:keyfind(<<"low">>, 1, Attrs),
    {High, Low}.

generic_test_() ->
    Rules = [{item, {generic, count, [{pre_encode, {?MODULE, to_jsx}},
                                      {post_decode, {?MODULE, from_jsx}}]}}],
    Record = {item, {15, 2}},
    {ok, E} = ejson_encode:encode(Record, Rules, []),
    {ok, D} = ejson_decode:decode(E, item, Rules, []),
    [{"Tuple is encoded as Json",
        [?_assertEqual({<<"high">>, 15}, jsx_path(E, "count.1")),
         ?_assertEqual({<<"low">>, 2}, jsx_path(E, "count.2"))]},
     {"Decoded as tuple",
        ?_assertMatch(Record, D)}
    ].

dict_to_entries(_Record, Dict) ->
    Entries = dict:fold(
                fun(K, V, Acc) ->
                        [{entry, K, V} | Acc]
                end, [], Dict),
    {entries, Entries}.

entries_to_dict({entries, EntryList}) ->
    lists:foldl(
      fun({entry, K, V}, Acc) ->
              dict:store(K, V, Acc)
      end, dict:new(), EntryList).

recursive_generic_test_() ->
    %% We are streaming a dict here, but during converting the key-value pairs
    %% we generate records which should be recursively converted.
    Rules = [{person, {string, name},
                      {generic, pets, [{pre_encode, {?MODULE, dict_to_entries}},
                                       {post_decode, {?MODULE, entries_to_dict}},
                                       recursive,
                                       {type, entries}]}},
             {entry, {string, name}, {string, value}},
             {entries, {list, entries, [{type, entry}]}}
            ],
    %% create the dict
    Pets = lists:foldl(
             fun({Name, Breed}, D) ->
                     dict:store(Name, Breed, D)
             end,
             dict:new(),
             [{"Kantor", "dog"}, {"Brownie", "cat"}, {"Killer", "lizard"}]),
    Record = {person, "Jimmy Lee", Pets},

    {ok, J} = ejson_encode:encode(Record, Rules, []),
    {ok, D} = ejson_decode:decode(J, person, Rules, []),
   
    [{"Dict KV as entries",
      [?_assertEqual(<<"Jimmy Lee">>, jsx_path(J, "name")),
       ?_assertEqual(<<"Kantor">>, jsx_path(J, "pets.entries.1.name")),
       ?_assertEqual(<<"Brownie">>, jsx_path(J, "pets.entries.2.name")),
       ?_assertEqual(<<"Killer">>, jsx_path(J, "pets.entries.3.name"))
      ]},
     {"Decoding to dict",
      ?_assertMatch({person, "Jimmy Lee", _}, D)}].
                      
