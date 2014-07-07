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
-module(ejson_encode).

-export([encode/2]).

%%------------------------------------------------------------------------------
%% Encoding rules
%%
%% -json({record_name, [Rules]}).
%%
%% Rule can be a:
%%   - string which will be the attribute name in json object
%%   - {atom, string}. String is the name of the attr, the value is an atom.
%%     It is useful when decoding since strings can be decoded either to
%%     a string or to an atom.
%%------------------------------------------------------------------------------


%% Convert a record
encode(Tuple, Opts) when is_tuple(Tuple) andalso is_atom(element(1, Tuple)) ->
    [RecordName | Values] = tuple_to_list(Tuple),
    %% Get field rules
    Fields = proplists:get_value(RecordName, Opts),
    %% Convert each values
    lists:reverse(
        lists:foldl(
            fun({Rule, Value}, Acc) ->
                case apply_rule(Rule, Tuple, Value, Opts) of
                    {AttrName, AttrValue} ->
                        [{list_to_binary(AttrName), AttrValue} | Acc];
                    undefined ->
                        Acc
                end
            end,
            [],
            zip(Fields, Values))).

apply_rule({atom, AttrName}, Tuple, Value, Opts) when is_atom(Value) ->
    {AttrName, list_to_binary(atom_to_list(Value))}.

zip([], []) ->
    [];
zip([H1|T1], []) ->
    [{H1, undefined} | zip(T1, [])];
zip([], [H2|T2]) ->
    [{undefined, H2} | zip([], T2)];
zip([H1|T1], [H2|T2]) ->
    [{H1, H2} | zip(T1, T2)].
