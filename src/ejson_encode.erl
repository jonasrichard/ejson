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

%% TODO: conditional macro for atom_to_binary
-define(BIN(Atom), list_to_binary(atom_to_list(Atom))).

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
    case ejson_util:get_fields(RecordName, Opts) of
        {error, _} = Error ->
            Error;
        Fields ->
            %% Convert each values
            Meta = [{<<"__rec">>, ?BIN(RecordName)}],
            case convert(ejson_util:zip(Fields, Values), Tuple, Opts, Meta) of
                {error, _} = Error ->
                    Error;
                AttrList ->
                    lists:reverse(AttrList)
            end
    end.

convert([], _Tuple, _Opts, Result) ->
    Result;
convert([{Name, Value} | T], Tuple, Opts, Result) ->
    %% Check duplicate field names
    case check_duplicate(Name, T) of
        false ->
            {NewName, NewValue} = apply_rule(Name, Tuple, Value, Opts),
            convert(T, Tuple, Opts, [{?BIN(NewName), NewValue} | Result]);
        FieldName ->
            {error, {duplicate_field_name, FieldName}}
    end.

apply_rule(AttrName, Tuple, Value, Opts) when is_number(Value) ->
    {AttrName, Value};
apply_rule({atom, AttrName}, Tuple, Value, Opts) when is_atom(Value) ->
    {AttrName, ?BIN(Value)};
apply_rule({binary, AttrName}, Tuple, Value, Opts) when is_binary(Value) ->
    {AttrName, Value};
apply_rule({string, AttrName}, Tuple, Value, Opts) ->
    {AttrName, unicode:characters_to_binary(Value)}.

check_duplicate({Spec, Name}, Fields) when Spec =:= atom orelse
                                           Spec =:= string orelse
                                           Spec =:= binary ->
    check_duplicate(Name, Fields);
check_duplicate(Name, Fields) ->
    case lists:member(Name, check_dup1(Fields)) of
        false ->
            false;
        _ ->
            Name
    end.

check_dup1([{{Spec, Name}, _Value} | T]) when Spec =:= atom orelse
                                              Spec =:= string orelse
                                              Spec =:= binary ->
    [Name | check_dup1(T)];
check_dup1([{Name, _Value} | T]) ->
    [Name | check_dup1(T)];
check_dup1([]) ->
    [].
