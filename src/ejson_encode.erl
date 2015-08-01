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

-include_lib("eunit/include/eunit.hrl").

%% TODO: conditional macro for atom_to_binary
-define(BIN(Name), if is_atom(Name) -> atom_to_binary(Name, utf8);
                      true          -> list_to_binary(Name)
                   end).

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

-spec encode(term(), list()) -> {ok, jsx:json_term()} |
                                {error, {duplicate_records, list(atom())}} |
                                {error, {duplicate_fields, list(binary())}}. 
encode(Value, Opts) ->
    case validate_rules(Opts) of
        ok ->
            case encode1(Value, Opts) of
                {error, _} = Error ->
                    Error;
                Result ->
                    {ok, Result}
            end;
        Error2 ->
            Error2
    end.

%% Convert a record
encode1(Tuple, Opts) when is_tuple(Tuple) andalso is_atom(element(1, Tuple)) ->
    [RecordName | Values] = tuple_to_list(Tuple),
    %% Get field rules
    case ejson_util:get_fields(RecordName, Opts) of
        {error, _} = Error ->
            Error;
        Fields ->
            %% Convert each values
            Meta = [{<<"__rec">>, atom_to_binary(RecordName, utf8)}],
            case convert(ejson_util:zip(Fields, Values), Tuple, Opts, Meta) of
                {error, _} = Error ->
                    Error;
                AttrList ->
                    lists:reverse(AttrList)
            end
    end;
encode1(Value, Opts) when is_list(Value) ->
    [encode1(Val, Opts) || Val <- Value];
encode1(Value, _Opts) when is_number(Value) orelse is_boolean(Value) ->
    Value;
encode1(undefined, _Opts) ->
    null.

validate_rules(Opts) ->
    RecordNames = [element(1, Opt) || Opt <- Opts],
    case lists:sort(RecordNames) -- lists:usort(RecordNames) of
        [] ->
            case check_duplicate_fields(Opts) of
                [] ->
                    ok;
                Fields ->
                    {error, {duplicate_fields, Fields}}
            end;
        Records ->
            {error, {duplicate_records, lists:usort(Records)}}
    end.


convert([], _Tuple, _Opts, Result) ->
    Result;
convert([{Name, Value} | T], Tuple, Opts, Result) ->
    case maybe_pre_process(Name, Tuple, Value) of
        {ok, PreProcessed} ->
            case apply_rule(Name, PreProcessed, Opts) of
                undefined ->
                    convert(T, Tuple, Opts, Result);
                {error, _} = Error ->
                    Error;
                {NewName, NewValue} ->
                    convert(T, Tuple, Opts, [{?BIN(NewName), NewValue} | Result])
            end;
        {error, _} = Error2 ->
            Error2
    end.

%% Generate jsx attribute from ejson field
apply_rule(Name, Value, Opts) ->
    case Name of
        skip ->
            undefined;
        {number, AttrName} ->
            number_rule(AttrName, Value);
        {number, AttrName, _FieldOpts} ->
            number_rule(AttrName, Value);
        {boolean, AttrName} ->
            boolean_rule(AttrName, Value);
        {boolean, AttrName, _FieldOpts} ->
            boolean_rule(AttrName, Value);
        {atom, AttrName} ->
            atom_rule(AttrName, Value);
        {atom, AttrName, _FieldOpts} ->
            atom_rule(AttrName, Value);
        {binary, AttrName} ->
            binary_rule(AttrName, Value);
        {binary, AttrName, _FieldOpts} ->
            binary_rule(AttrName, Value);
        {string, AttrName} ->
            string_rule(AttrName, Value);
        {string, AttrName, _FieldOpts} ->
            string_rule(AttrName, Value);
        {record, AttrName} ->
            record_rule(AttrName, Value, [], Opts);
        {record, AttrName, FieldOpts} ->
            record_rule(AttrName, Value, FieldOpts, Opts);
        {list, AttrName} ->
            list_rule(AttrName, Value, Opts);
        {list, AttrName, _FieldOpts} ->
            list_rule(AttrName, Value, Opts);
        {generic, AttrName, _FieldOpts} ->
            %% Generic encoding is handled in pre_process phase
            {AttrName, Value};
        {const, AttrName, Const} ->
            {AttrName, encode1(Const, Opts)};
        AttrName ->
            {error, {invalid_field_rule, AttrName, Name}}
    end.

boolean_rule(AttrName, undefined) ->
    {AttrName, null};
boolean_rule(AttrName, Value) when is_boolean(Value) ->
    {AttrName, Value};
boolean_rule(AttrName, Value) ->
    {error, {boolean_value_expected, AttrName, Value}}.

number_rule(AttrName, undefined) ->
    {AttrName, null};
number_rule(AttrName, Value) when is_number(Value) ->
    {AttrName, Value};
number_rule(AttrName, Value) ->
    {error, {numeric_value_expected, AttrName, Value}}.

atom_rule(AttrName, undefined) ->
    {AttrName, null};
atom_rule(AttrName, Value) when is_atom(Value) ->
    {AttrName, atom_to_binary(Value, utf8)};
atom_rule(AttrName, Value) ->
    {error, {atom_value_expected, AttrName, Value}}.

binary_rule(AttrName, undefined) ->
    {AttrName, null};
binary_rule(AttrName, Value) when is_binary(Value) ->
    {AttrName, Value};
binary_rule(AttrName, Value) ->
    {error, {binary_value_expected, AttrName, Value}}.

string_rule(AttrName, undefined) ->
    {AttrName, null};
string_rule(AttrName, Value) when is_list(Value) ->
    {AttrName, unicode:characters_to_binary(Value)};
string_rule(AttrName, Value) ->
    {error, {string_value_expected, AttrName, Value}}.

record_rule(AttrName, undefined, _FieldOpts, _Opts) ->
    {AttrName, null};
record_rule(AttrName, Value, _FieldOpts, Opts) when is_tuple(Value) ->
    {AttrName, encode1(Value, Opts)};
record_rule(AttrName, Value, _FieldOpts, _Opts) ->
    {error, {record_value_expected, AttrName, Value}}.

list_rule(AttrName, undefined, _Opts) ->
    {AttrName, null};
list_rule(AttrName, Value, Opts) when is_list(Value) ->
    List = [encode1(V, Opts) || V <- Value],
    {AttrName, List};
list_rule(AttrName, Value, _Opts) ->
    {error, {list_value_expected, AttrName, Value}}.

maybe_pre_process({const, _Name, _Const}, _Tuple, Value) ->
    {ok, Value};
maybe_pre_process({Type, Name, FieldOpts}, Tuple, Value) ->
    case lists:keyfind(pre_encode, 1, FieldOpts) of
        false ->
            case Type of
                generic ->
                    %% In case of generic pre_encode is mandatory
                    {error, {no_pre_encode, Name, Value}};
                _ ->
                    {ok, Value}
            end;
        {pre_encode, {M, F}} ->
            try erlang:apply(M, F, [Tuple, Value]) of
                Val ->
                    {ok, Val}
            catch
                E:R ->
                    {error, {Name, E, R}}
            end
    end;
maybe_pre_process(_Rule, _Tuple, Value) ->
    {ok, Value}.

%% Check duplicate fields in record definition. It gives false if each field is
%% unique, otherwise it gives the duplicate field names.
-spec check_duplicate_fields(list()) -> false | list(atom()).
check_duplicate_fields([]) ->
    [];
check_duplicate_fields([Rule | Rules]) ->
    [_ | Fields] = tuple_to_list(Rule),
    FieldNames = [field_name(Field) || Field <- Fields],
    Names = [F || F <- FieldNames, F =/= undefined],
    case lists:sort(Names) -- lists:usort(Names) of
        [] ->
            check_duplicate_fields(Rules);
        DupFields ->
            DupFields
    end.

field_name(Field) ->
    case ejson_util:get_field_name(Field) of
        undefined ->
            undefined;
        Name when is_atom(Name) ->
            ejson_util:atom_to_binary_cc(Name);
        List when is_list(List) ->
            list_to_binary(List)
    end.
