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

-export([encode/3]).

-spec encode(term(), list(), list()) ->
    {ok, jsx:json_term()} |
    {error, {duplicate_records, list(atom())}} |
    {error, {duplicate_fields, list(binary())}}.
encode(Value, Rules, Opts) ->
    case validate_rules(Rules) of
        ok ->
            case encode1(Value, Rules, Opts) of
                {error, _} = Error ->
                    Error;
                Result ->
                    {ok, Result}
            end;
        Error2 ->
            Error2
    end.

%% Convert a record
encode1(Tuple, Rules, Opts) when is_tuple(Tuple) andalso
                                 is_atom(element(1, Tuple)) ->
    [RecordName | Values] = tuple_to_list(Tuple),
    %% Get field rules
    case ejson_util:get_fields(RecordName, Rules) of
        {error, _} = Error ->
            Error;
        Fields ->
            %% Convert each values
            FV = ejson_util:zip(Fields, Values),
            case convert(FV, Tuple, Rules, Opts, []) of
                {error, _} = Error ->
                    Error;
                AttrList ->
                    %% Put __rec type information if record type is
                    %% in the {type, Records} lists in the Opts
                    case is_typed_record(RecordName, Opts) of
                        true ->
                            AL = add_rec_type(RecordName, AttrList),
                            lists:reverse(AL);
                        false ->
                            lists:reverse(AttrList)
                    end
            end
    end;
encode1(Value, Rules, Opts) when is_list(Value) ->
    [encode1(Val, Rules, Opts) || Val <- Value];
encode1(Value, _Rules, _Opts) when is_number(Value) orelse is_boolean(Value) ->
    Value;
encode1(undefined, _Rules, _Opts) ->
    null.

convert([], _Tuple, _Rules, _Opts, Result) ->
    Result;
convert([{Name, Value} | T], Tuple, Rules, Opts, Result) ->
    case maybe_pre_process(Name, Tuple, Value) of
        {ok, PreProcessed} ->
            case maybe_apply_rule(Name, PreProcessed, Rules, Opts) of
                undefined ->
                    convert(T, Tuple, Rules, Opts, Result);
                {error, _} = Error ->
                    Error;
                {ok, {NewName, NewValue}} when is_atom(NewName) ->
                    NewPair = {atom_to_binary(NewName, utf8), NewValue},
                    convert(T, Tuple, Rules, Opts, [NewPair | Result]);
                {ok, {NewName, NewValue}} ->
                    NewPair = {list_to_binary(NewName), NewValue},
                    convert(T, Tuple, Rules, Opts, [NewPair | Result])
            end;
        {error, _} = Error2 ->
            Error2
    end.

%% Generate jsx attribute from ejson field
maybe_apply_rule({_, _, FieldOpts} = Name, undefined = Value, Rules, Opts) ->
    case lists:keyfind(default, 1, FieldOpts) of
        {_, undefined} ->
            undefined;
        _ ->
            apply_rule(Name, Value, Rules, Opts)
    end;
maybe_apply_rule(Name, Value, Rules, Opts) ->
    apply_rule(Name, Value, Rules, Opts).

apply_rule(Name, Value, Rules, Opts) ->
    case Name of
        skip ->
            undefined;
        {skip, _FieldOpts} ->
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
        {binary, AttrName, FieldOpts} ->
            binary_rule(AttrName, Value, FieldOpts);
        {string, AttrName} ->
            string_rule(AttrName, Value);
        {string, AttrName, _FieldOpts} ->
            string_rule(AttrName, Value);
        {record, AttrName} ->
            record_rule(AttrName, Value, [], Rules, Opts);
        {record, AttrName, FieldOpts} ->
            record_rule(AttrName, Value, FieldOpts, Rules, Opts);
        {list, AttrName} ->
            mixed_list_rule(AttrName, Value, Rules, Opts);
        {list, AttrName, _FieldOpts} ->
            list_rule(AttrName, Value, Rules, Opts);
        {map, AttrName, FieldOpts} ->
            map_rule(AttrName, Value, FieldOpts, Rules, Opts);
        {generic, AttrName, FieldOpts} ->
            generic_rule(AttrName, Value, FieldOpts, Rules, Opts);
        {virtual, AttrName, _FieldOpts} ->
            %% The pre_encode already ran
            {ok, {AttrName, Value}};
        {const, AttrName, Const} ->
            {ok, {AttrName, encode1(Const, Rules, Opts)}};
        AttrName ->
            {error, {invalid_field_rule, AttrName, Name}}
    end.

boolean_rule(AttrName, undefined) ->
    {ok, {AttrName, null}};
boolean_rule(AttrName, Value) when is_boolean(Value) ->
    {ok, {AttrName, Value}};
boolean_rule(AttrName, Value) ->
    {error, {boolean_value_expected, AttrName, Value}}.

number_rule(AttrName, undefined) ->
    {ok, {AttrName, null}};
number_rule(AttrName, Value) when is_number(Value) ->
    {ok, {AttrName, Value}};
number_rule(AttrName, Value) ->
    {error, {numeric_value_expected, AttrName, Value}}.

atom_rule(AttrName, undefined) ->
    {ok, {AttrName, null}};
atom_rule(AttrName, Value) when is_atom(Value) ->
    {ok, {AttrName, atom_to_binary(Value, utf8)}};
atom_rule(AttrName, Value) ->
    {error, {atom_value_expected, AttrName, Value}}.

binary_rule(AttrName, undefined) ->
    {ok, {AttrName, null}};
binary_rule(AttrName, Value) when is_binary(Value) ->
    {ok, {AttrName, Value}};
binary_rule(AttrName, Value) ->
    {error, {binary_value_expected, AttrName, Value}}.

binary_rule(AttrName, undefined, _FieldOpts) ->
    binary_rule(AttrName, undefined);
binary_rule(AttrName, Value, FieldOpts) when is_binary(Value) ->
    case lists:member(base64, FieldOpts) of
        false ->
            {ok, {AttrName, Value}};
        true ->
            {ok, {AttrName, base64:encode(Value)}}
    end;
binary_rule(AttrName, Value, _FieldOpts) ->
    binary_rule(AttrName, Value).

string_rule(AttrName, undefined) ->
    {ok, {AttrName, null}};
string_rule(AttrName, Value) when is_list(Value) ->
    {ok, {AttrName, unicode:characters_to_binary(Value)}};
string_rule(AttrName, Value) ->
    {error, {string_value_expected, AttrName, Value}}.

record_rule(AttrName, undefined, _FieldOpts, _Rules, _Opts) ->
    {ok, {AttrName, null}};
record_rule(AttrName, Value, FieldOpts, Rules, Opts) when is_tuple(Value) ->
    case maybe_external_record_rule(Value, FieldOpts, Rules, Opts) of
        {error, {no_such_record, Actual}} = E2 ->
            case lists:keyfind(type, 1, FieldOpts) of
                false ->
                    E2;
                {type, Expected} ->
                    {error, {mismatched_record, Actual, Expected}}
            end;
        {error, _} = E ->
            E;
        AttrList ->
            case lists:keyfind(type, 1, FieldOpts) of
                false ->
                    %% If record type is not specified add __rec meta data
                    {ok, {AttrName, add_rec_type(element(1, Value), AttrList)}};
                _ ->
                    {ok, {AttrName, AttrList}}
            end
    end;
record_rule(AttrName, Value, _FieldOpts, _Rules, _Opts) ->
    {error, {record_value_expected, AttrName, Value}}.

maybe_external_record_rule(Value, FieldOpts, Rules, Opts) ->
    case lists:keyfind(module, 1, FieldOpts) of
        false ->
            %% Internal record, defined in the same module
            encode1(Value, Rules, Opts);
        {module, Module} ->
            {Rules1, Opts1} = ejson:json_modules(Module),
            case encode1(Value, Rules1, Opts1) of
                {error, _} = E ->
                    E;
                Result ->
                    Result
            end
    end.

map_rule(AttrName, undefined, _FieldOpts, _Rules, _Opts) ->
    {ok, {AttrName, null}};
map_rule(AttrName, Value, _FieldOpts, _Rules, _Opts) ->
    {ok, {AttrName, Value}}.

generic_rule(AttrName, undefined, _FieldOpts, _Rules, _Opts) ->
    {ok, {AttrName, null}};
generic_rule(AttrName, Value, FieldOpts, Rules, Opts) ->
    case lists:member(recursive, FieldOpts) of
        false ->
            %% If there is no recursive rule Value is expected to be
            %% a jsx:term()
            {ok, {AttrName, Value}};
        true ->
            %% Apply the rules recursively
            case encode1(Value, Rules, Opts) of
                {error, _} = E ->
                    E;
                AttrList ->
                    case lists:keyfind(type, 1, FieldOpts) of
                        false ->
                            {error, {type_required, AttrName, FieldOpts}};
                        {type, _Type} ->
                            %% Check if {<<"__rec", Type} there 
                            {ok, {AttrName, AttrList}}
                    end
            end
    end.

list_rule(AttrName, undefined, _Rules, _Opts) ->
    {ok, {AttrName, null}};
list_rule(AttrName, Value, Rules, Opts) when is_list(Value) ->
    List = [encode1(V, Rules, Opts) || V <- Value],
    {ok, {AttrName, List}};
list_rule(AttrName, Value, _Rules, _Opts) ->
    {error, {list_value_expected, AttrName, Value}}.

mixed_list_rule(AttrName, undefined, _Rules, _Opts) ->
    {ok, {AttrName, null}};
mixed_list_rule(AttrName, Value, Rules, Opts) when is_list(Value) ->
    try lists:map(
          fun(N) when is_number(N) ->
                  N;
             (B) when is_boolean(B) ->
                  B;
             (T) when is_tuple(T) ->
                  Rec = element(1, T),
                  case encode1(T, Rules, Opts) of
                      {error, R} ->
                          throw(R);
                      AttrList ->
                          add_rec_type(Rec, AttrList)
                  end;
             (E) ->
                  throw({invalid_list_item, E})
          end, Value) of
        List ->
            {ok, {AttrName, List}}
    catch
        E:R ->
            {error, AttrName, E, R}
    end;
mixed_list_rule(AttrName, Value, _Rules, _Opts) ->
    {error, {list_value_expected, AttrName, Value}}.

maybe_pre_process({const, _Name, _Const}, _Tuple, Value) ->
    {ok, Value};
maybe_pre_process({virtual, Name, FieldOpts}, Tuple, _Value) ->
    case lists:keyfind(pre_encode, 1, FieldOpts) of
        false ->
            {error, {no_pre_encode, Name}};
        {pre_encode, Fun} ->
            %% In case of virtual only the tuple is passed
            safe_call_fun(Name, [Tuple], Fun)
    end;
maybe_pre_process({map, Name, FieldOpts}, Tuple, Value) ->
    case lists:keyfind(pre_encode, 1, FieldOpts) of
        false ->
            {error, {no_pre_encode, Name, Value}};
        {pre_encode, Fun} ->
            safe_call_fun(Name, [Tuple, Value], Fun)
    end;
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
        {pre_encode, Fun} ->
            safe_call_fun(Name, [Tuple, Value], Fun)
    end;
maybe_pre_process(_Rule, _Tuple, Value) ->
    {ok, Value}.

safe_call_fun(Name, Args, {M, F}) ->
    try erlang:apply(M, F, Args) of
        Val ->
            {ok, Val}
    catch
        E:R ->
            {error, {Name, E, R, Args}}
    end.

add_rec_type(Type, List) ->
    case lists:keyfind(<<"__rec">>, 1, List) of
        false ->
            [{<<"__rec">>, atom_to_binary(Type, utf8)} | List];
        _ ->
            List
    end.

is_typed_record(RecordName, Opts) ->
    case lists:keyfind(type_field, 1, Opts) of
        false ->
            false;
        {_, Types} ->
            lists:member(RecordName, Types)
    end.

validate_rules(Rules) ->
    RecordNames = [element(1, Rule) || Rule <- Rules],
    case lists:sort(RecordNames) -- lists:usort(RecordNames) of
        [] ->
            case check_duplicate_fields(Rules) of
                [] ->
                    ok;
                Fields ->
                    {error, {duplicate_fields, Fields}}
            end;
        Records ->
            {error, {duplicate_records, lists:usort(Records)}}
    end.

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
