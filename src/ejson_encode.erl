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
-define(BIN(Name), if is_atom(Name) -> list_to_binary(atom_to_list(Name));
                      true          -> list_to_binary(Name)
                          %%{error, {atom_expected, Name}}
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
            Meta = [{<<"__rec">>, ?BIN(RecordName)}],
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
    case apply_rule(Name, Tuple, Value, Opts) of
        undefined ->
            convert(T, Tuple, Opts, Result);
        {error, _} = Error ->
            Error;
        {NewName, NewValue} ->
            convert(T, Tuple, Opts, [{?BIN(NewName), NewValue} | Result])
    end.

apply_rule(skip, _Tuple, _Value, _Opts) ->
    undefined;
apply_rule({atom, AttrName}, _Tuple, Value, _Opts) when is_atom(Value) ->
    {AttrName, ?BIN(Value)};
apply_rule({atom, AttrName}, _Tuple, Value, _Opts) ->
    {error, {atom_value_expected, AttrName, Value}};
apply_rule({binary, AttrName}, _Tuple, Value, _Opts) when is_binary(Value) ->
    {AttrName, Value};
apply_rule({binary, AttrName}, _Tuple, Value, _Opts) ->
    {error, {binary_value_expected, AttrName, Value}};
apply_rule({string, AttrName}, _Tuple, Value, _Opts) ->
    {AttrName, unicode:characters_to_binary(Value)};
apply_rule({list, AttrName}, _Tuple, Value, Opts) when is_list(Value) ->
    List = [encode1(V, Opts) || V <- Value],
    {AttrName, List};
apply_rule({list, AttrName}, _Tuple, Value, _Opts) ->
    {error, {list_value_expected, AttrName, Value}};
apply_rule({list, AttrName, _Type}, Tuple, Value, Opts) ->
    apply_rule({list, AttrName}, Tuple, Value, Opts);
apply_rule({proplist, AttrName}, _Tuple, Value, _Opts) ->
    Vals = lists:map(
      fun({Prop, Val}) ->
              {ejson_util:atom_to_binary_cc(Prop), Val};
         (Prop) ->
              {ejson_util:atom_to_binary_cc(Prop), true}
      end, Value),
    {AttrName, [{<<"__type">>, <<"proplist">>} | Vals]};
apply_rule({field_fun, AttrName, {M, F}, _}, _Tuple, Value, Opts) ->
    Val = erlang:apply(M, F, [Value]),
    {AttrName, encode1(Val, Opts)};
apply_rule({field_fun, AttrName, EncFun, _}, _Tuple, Value, Opts) ->
    Val = EncFun(Value),
    {AttrName, encode1(Val, Opts)};
apply_rule({rec_fun, AttrName, {M, F}}, Tuple, _Value, Opts) ->
    Val = erlang:apply(M, F, [Tuple]),
    {AttrName, encode1(Val, Opts)};
apply_rule({rec_fun, AttrName, EncFun}, Tuple, _Value, Opts) ->
    Val = EncFun(Tuple),
    {AttrName, encode1(Val, Opts)};
apply_rule({const, AttrName, Const}, _Tuple, _Value, Opts) ->
    {AttrName, encode1(Const, Opts)};
apply_rule(AttrName, _Tuple, Value, Opts) when is_tuple(Value) ->
    {AttrName, encode1(Value, Opts)};
apply_rule(AttrName, _Tuple, Value, _Opts) when is_number(Value) ->
    {AttrName, Value}.

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
