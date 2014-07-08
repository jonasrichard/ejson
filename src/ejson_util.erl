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
         is_convertable_atom/1]).

atom_to_binary_cc(Atom) ->
    list_to_binary(lists:reverse(camel_case(atom_to_list(Atom), []))).

binary_to_atom_cc(Binary) ->
    list_to_atom(lists:reverse(underscore(binary_to_list(Binary), []))).

is_convertable_atom(Atom) ->
    %% true if the atom can be converted by the two functions unambiguously
    L = atom_to_list(Atom),
    start_with_char(L) andalso proper_underscore(L).

start_with_char([L|_]) when L >= $a andalso L =< $z ->
    true;
start_with_char(_) ->
    false.

%% If there is an underscore, it needs to follow by a letter
proper_underscore([]) ->
    true;
proper_underscore([$_, L | T]) when L >= $a andalso L =< $z ->
    proper_underscore(T);
proper_underscore([$_ | T]) ->
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
