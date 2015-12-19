%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2013-2014, Richard Jonas <mail@jonasrichard.hu>
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @author Richard Jonas <mail@jonasrichard.hu>
%%% @doc
%%% ejson encoder module
%%% @end
%%%-----------------------------------------------------------------------------
-module(ejson_util).

-export([atom_to_binary_cc/1,
         binary_to_atom_cc/1,
         is_name_convertable/1,
         get_fields/2,
         get_field_name/1,
         zip/2
        ]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

atom_to_binary_cc(Atom) ->
    list_to_binary(lists:reverse(camel_case(atom_to_list(Atom), []))).

binary_to_atom_cc(Binary) ->
    list_to_atom(lists:reverse(underscore(binary_to_list(Binary), []))).

%% true if the atom can be converted by the two functions unambiguously
is_name_convertable(Atom) when is_atom(Atom) ->
    is_name_convertable(atom_to_list(Atom));
is_name_convertable(String) ->
    start_with_char(String) andalso proper_underscore(String).

%%-----------------------------------------------------------------------------
%% @doc Get attributes of a record, or error if no such record.
%% @end
%%-----------------------------------------------------------------------------
-spec get_fields(atom(), term()) -> list() | {error, {no_such_record, atom()}}.
get_fields(RecordName, Opts) ->
    case lists:keyfind(RecordName, 1, Opts) of
        false ->
            {error, {no_such_record, RecordName}};
        R ->
            [_ | Fields] = tuple_to_list(R),
            Fields
    end.

get_field_name(skip) ->
    undefined;
get_field_name({skip, _FieldOpts}) ->
    undefined;
get_field_name({Type, Field}) when Type =:= atom orelse
                                   Type =:= binary orelse
                                   Type =:= boolean orelse
                                   Type =:= list orelse
                                   Type =:= number orelse
                                   Type =:= proplist orelse
                                   Type =:= record orelse
                                   Type =:= string ->
    Field;
get_field_name({Type, Field, _FieldOpts}) when Type =:= atom orelse
                                               Type =:= binary orelse
                                               Type =:= boolean orelse
                                               Type =:= const orelse
                                               Type =:= list orelse
                                               Type =:= map orelse
                                               Type =:= number orelse
                                               Type =:= record orelse
                                               Type =:= string orelse
                                               Type =:= virtual ->
    Field;
get_field_name({generic, Field, _FieldOpts}) ->
    Field;
get_field_name(Field) ->
    {error, {invalid_field_rule, Field}}.

zip([], []) ->
    [];
zip([H1|T1], []) ->
    [{H1, undefined} | zip(T1, [])];
zip([], [H2|T2]) ->
    [{undefined, H2} | zip([], T2)];
zip([H1|T1], [H2|T2]) ->
    [{H1, H2} | zip(T1, T2)].

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

start_with_char([L|_]) when L >= $a andalso L =< $z ->
    true;
start_with_char(_) ->
    false.

%% If there is an underscore, it needs to follow by a letter
proper_underscore([]) ->
    true;
proper_underscore([$_, L | T]) when L >= $a andalso L =< $z ->
    proper_underscore(T);
proper_underscore([$_ | _T]) ->
    false;
proper_underscore([L | _T]) when L >= $A andalso L =< $Z ->
    false;
proper_underscore([_ | T]) ->
    proper_underscore(T).

camel_case([], R) ->
    R;
camel_case([L], R) ->
    [L|R];
camel_case([$_, L | T], R) ->
    camel_case(T, [string:to_upper(L) | R]);
camel_case([H | T], R) ->
    camel_case(T, [H | R]).

underscore([], R) ->
    R;
underscore([Cap | T], R) when Cap >= $A andalso Cap =< $Z ->
    underscore(T, [Cap + 32, $_ | R]);
underscore([Low | T], R) ->
    underscore(T, [Low | R]).
