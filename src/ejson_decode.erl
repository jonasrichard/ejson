%%%-------------------------------------------------------------------
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
%%% ejson decoder module
%%% @end
%%%-------------------------------------------------------------------
-module(ejson_decode).

-export([decode/2,
         decode/3]).

%% TODO:
%% exact_value should return with {ok, Value} or {error, Reason}, so
%% every time when we extract, we also need to case pattern match.
%% So in the end we don't need decode and decode1 wrappers to generate
%% {ok, Value}, etc.

%%-----------------------------------------------------------------------------
%% @doc Decode JSON from jsx attrlist.
%% Calling this function AttrList has one such element which contains type
%% information about the target type (__rec or so).
%% @end
%%-----------------------------------------------------------------------------
decode(AttrList, Opts) ->
    case decode1(AttrList, Opts) of
        {error, _} = Error ->
            Error;
        Result ->
            {ok, Result}
    end.

decode(AttrList, Opts, RecordName) ->
    case decode1(AttrList, Opts, RecordName) of
        {error, _} = Error ->
            Error;
        Result ->
            {ok, Result}
    end.

decode1(AttrList, Opts) ->
    case lists:keyfind(<<"__rec">>, 1, AttrList) of
        {_, Rec} ->
            RecordName = list_to_atom(binary_to_list(Rec)),
            decode1(AttrList, Opts, RecordName);
        false ->
            {error, no_record_name}
    end.

%% When we have proper target record name (3rd parameter as atom)
decode1(AttrList, Opts, RecordName) ->
    case ejson_util:get_fields(RecordName, Opts) of
        {error, _} = Error ->
            Error;
        Fields ->
            case extract_fields(Fields, AttrList, Opts) of
                {error, _} = Error ->
                    Error;
                Values ->
                    list_to_tuple([RecordName | Values])
            end
    end.

extract_fields([], _, _) ->
    [];
extract_fields([Field | F], AttrList, Opts) ->
    case ejson_util:get_field_name(Field) of
        undefined ->
            %% The skip rule: we haven't included that field in json,
            %% so we cannot extract any value for it.
            [undefined | extract_fields(F, AttrList, Opts)];
        BareField ->
            Bf = if is_atom(BareField) ->
                        atom_to_binary(BareField, utf8);
                    true ->
                        list_to_binary(BareField)
                 end,
            case lists:keyfind(Bf, 1, AttrList) of
                false ->
                    %% No value for field, check if we have default
                    case default_value(Field) of
                        undefined ->
                            {error, {no_value_for, Field}};
                        DefVal ->
                            [DefVal | extract_fields(F, AttrList, Opts)]
                    end;
                {_, Value} ->
                    %% Extract value based on Field rule
                    Extracted = extract_value(Field, Value, Opts),
                    [Extracted | extract_fields(F, AttrList, Opts)]
            end
    end.

extract_value(Rule, Value, Opts) ->
    case Rule of
        {atom, _} ->
            extract_atom(Value);
        {atom, _, _} ->
            extract_atom(Value);
        {binary, _} ->
            Value;
        {binary, _, _} ->
            Value;
        {string, _} ->
            extract_string(Value);
        {string, _, _} ->
            extract_string(Value);
        {record, _} ->
            extract_record(Value, [], Opts);
        {record, _, FieldOpts} ->
            extract_record(Value, FieldOpts, Opts);
        {list, _} ->
            extract_list(Value, [], Opts);
        {list, _, FieldOpts} ->
            extract_list(Value, FieldOpts, Opts);
        {field_fun, _, _EncFun, DecFun} ->
            extract_field_fun(Value, DecFun, Value, Opts);
        {rec_fun, _, _} ->
            undefined;
        {proplist, _} ->
            %% TODO proper conversion here!
            undefined;
        {const, _, _} ->
            undefined;
        _AttrName when is_list(Value) ->
            decode1(Value, Opts);
        _AttrName ->
            %% number and boolean case
            Value
    end.

extract_atom(null) ->
    undefined;
extract_atom(Value) ->
    binary_to_atom(Value, utf8).

extract_string(null) ->
    undefined;
extract_string(Value) ->
    unicode:characters_to_list(Value, utf8).

extract_record(null, FieldOpts, Opts) ->
    case proplists:get_value(default, FieldOpts) of
        undefined ->
            undefined;
        Default ->
            extract_record(Default, FieldOpts, Opts)
    end;
extract_record(Value, FieldOpts, Opts) ->
    case proplists:get_value(type, FieldOpts) of
        undefined ->
            decode1(Value, Opts);
        Type ->
            decode1(Value, Opts, Type)
    end.

extract_list(null, _FieldOpts, _Opts) ->
    undefined;
extract_list(Value, FieldOpts, Opts) ->
    case proplists:get_value(type, FieldOpts) of
        undefined ->
            %% No target type for list element, it can be an attrlist
            %% or a primitive value
            lists:map(
              fun(V) when is_list(V) ->
                      {ok, D} = decode(V, Opts),
                      %% TODO make an error case and gives back error
                      D;
                 (V) ->
                      V
              end, Value);
        Type ->
            [decode1(V, Opts, Type) || V <- Value]
    end.

extract_field_fun(Value, {M, F}, Value, _Opts) ->
    try erlang:apply(M, F, [Value]) of
        Val ->
            Val
            %%decode1(Val, Opts)
    catch
        E:R ->
            {error, {field_run, {M, F}, {E, R}}}
    end.

%% Get the default value from a field rule
default_value({Type, _, Opts}) when Type =:= atom orelse
                                    Type =:= binary orelse
                                    Type =:= list orelse
                                    Type =:= record orelse
                                    Type =:= string ->
    proplists:get_value(default, Opts);
default_value(_) ->
    undefined.
