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

-export([decode/4]).

%%-----------------------------------------------------------------------------
%% @doc Decode JSON from jsx attrlist.
%% Calling this function AttrList has one such element which contains type
%% information about the target type (__rec or so).
%% @end
%%-----------------------------------------------------------------------------
decode([H | _] = List, RecordName, Rules, Opts) when is_list(H) ->
    R = lists:foldl(
          fun(AttrList, Acc) when is_list(Acc) ->
                  case decode1(AttrList, RecordName, Rules, Opts) of
                      {error, _} = E ->
                          E;
                      Record ->
                          [Record | Acc]
                  end;
             (_, {error, _} = E) ->
                  E
          end, [], List),
    case R of
        _ when is_list(R) ->
            lists:reverse(R);
        _ ->
            R
    end;
decode(AttrList, RecordName, Rules, Opts) ->
    case decode1(AttrList, RecordName, Rules, Opts) of
        {error, _} = Error ->
            Error;
        Result ->
            {ok, Result}
    end.

decode1([H | _] = List, Rules, Opts) when is_list(H) ->
    %% List of list of name/value pairs is a list of objects in jsx
    lists:foldl(
      fun(AttrList, Acc) when is_list(Acc) ->
              case decode1(AttrList, Rules, Opts) of
                  {error, _} = E ->
                      E;
                  Record ->
                      [Record | Acc]
              end;
         (_, {error, _} = E) ->
              %% If we get an error, don't go on with processing
              E
      end, [], List);
decode1(AttrList, Rules, Opts) ->
    case lists:keyfind(<<"__rec">>, 1, AttrList) of
        {_, Rec} ->
            RecordName = binary_to_atom(Rec, utf8),
            decode1(AttrList, RecordName, Rules, Opts);
        false ->
            {error, no_record_name}
    end.

%% When we have proper target record name (3rd parameter as atom)
decode1(AttrList, RecordName, Rules, Opts) ->
    case ejson_util:get_fields(RecordName, Rules) of
        {error, _} = Error ->
            Error;
        Fields ->
            case extract_fields(Fields, AttrList, Rules, Opts, []) of
                {error, _} = Error ->
                    Error;
                Values ->
                    list_to_tuple([RecordName | Values])
            end
    end.

%% TODO refactor this method
%% This function should be tail-recursive because virtual rule needs to
%% get the whole record what we have so far
extract_fields([], _, _, _, Result) ->
    Result;
extract_fields([Field | F], AttrList, Rules, Opts, Result) ->
    case ejson_util:get_field_name(Field) of
        undefined ->
            %% The skip rule: we haven't included that field in json,
            %% so we cannot extract any value for it. However, if we have
            %% default value, let us specify it.
            case default_value(Field) of
                false ->
                    extract_fields(F, AttrList, Rules, Opts, Result ++ [undefined]);
                {_, DefValue} ->
                    extract_fields(F, AttrList, Rules, Opts, Result ++ [DefValue])
            end;
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
                        false ->
                            {error, {no_value_for, Field}};
                        {_, DefVal} ->
                            extract_fields(F, AttrList, Rules, Opts, Result ++ [DefVal])
                    end;
                {_, Value} ->
                    %% Extract value based on Field rule
                    case extract_value(Field, Value, Rules, Opts) of
                        {error, _} = Error ->
                            Error;
                        {ok, Extracted} ->
                            case maybe_post_process(Field, Result, Extracted) of
                                {ok, NewRes, NewVal} ->
                                    extract_fields(F, AttrList, Rules, Opts, NewRes ++ [NewVal]);
                                {ok, NewRes} ->
                                    extract_fields(F, AttrList, Rules, Opts, NewRes);
                                {error, _} = Error2 ->
                                    Error2
                            end
                    end
            end
    end.

extract_value(Rule, Value, Rules, Opts) ->
    case Rule of
        {atom, _} ->
            extract_atom(Value);
        {atom, _, _} ->
            extract_atom(Value);
        {binary, _} ->
            extract_binary(Value);
        {binary, _, FieldOpts} ->
            extract_binary(Value, FieldOpts);
        {boolean, _} ->
            extract_boolean(Value);
        {boolean, _, _} ->
            extract_boolean(Value);
        {number, _} ->
            extract_number(Value);
        {number, _, _} ->
            extract_number(Value);
        {string, _} ->
            extract_string(Value);
        {string, _, _} ->
            extract_string(Value);
        {record, _} ->
            extract_record(Value, [], Rules, Opts);
        {record, _, FieldOpts} ->
            extract_record(Value, FieldOpts, Rules, Opts);
        {list, _} ->
            extract_list(Value, [], Rules, Opts);
        {list, _, FieldOpts} ->
            extract_list(Value, FieldOpts, Rules, Opts);
        {map, Name, FieldOpts} ->
            extract_map(Name, Value, FieldOpts, Rules, Opts);
        {generic, Name, FieldOpts} ->
            extract_generic(Name, Value, FieldOpts, Rules, Opts);
        {virtual, _, _} ->
            {ok, Value};
        {const, _, _} ->
            {ok, undefined}
    end.

extract_atom(null) ->
    {ok, undefined};
extract_atom(Value) ->
    try binary_to_atom(Value, utf8) of
        Atom ->
            {ok, Atom}
    catch
        error:badarg ->
            {error, {illegal_characters_in_atom, Value}}
    end.

extract_binary(null) ->
    {ok, undefined};
extract_binary(Value) ->
    {ok, Value}.

extract_binary(null, _FieldOpts) ->
    {ok, undefined};
extract_binary(Value, FieldOpts) ->
    case lists:member(base64, FieldOpts) of
        false ->
            {ok, Value};
        true ->
            try
                {ok, base64:decode(Value)}
            catch
                _:_ ->
                    {error, {illegal_base64_binary, Value}}
            end
    end.

extract_boolean(null) ->
    {ok, undefined};
extract_boolean(Value) ->
    {ok, Value}.

extract_number(null) ->
    {ok, undefined};
extract_number(Value) ->
    {ok, Value}.

extract_string(null) ->
    {ok, undefined};
extract_string(Value) ->
    {ok, unicode:characters_to_list(Value, utf8)}.

extract_record(null, FieldOpts, Rules, Opts) ->
    case proplists:get_value(default, FieldOpts) of
        undefined ->
            {ok, undefined};
        Default ->
            extract_record(Default, FieldOpts, Rules, Opts)
    end;
extract_record(Value, FieldOpts, Rules, Opts) ->
    Type = proplists:get_value(type, FieldOpts),
    case proplists:get_value(module, FieldOpts) of
        undefined ->
            case Type of
                undefined ->
                    %% TODO error handling
                    {ok, decode1(Value, Rules, Opts)};
                _ ->
                    {ok, decode1(Value, Type, Rules, Opts)}
            end;
        Module ->
            extract_external_record(Value, Type, Module)
    end.

extract_external_record(Value, Type, Module) ->
    {Rules, Opts} = ejson:json_modules(Module),
    case Type of
        undefined ->
            {ok, decode1(Value, Rules, Opts)};
        _ ->
            {ok, decode1(Value, Type, Rules, Opts)}
    end.

extract_map(_Name, null, _FieldOpts, _Rules, _Opts) ->
    {ok, undefined};
extract_map(_Name, Value, _FieldOpts, _Rules, _Opts) ->
    {ok, Value}.

extract_generic(_Name, null, _FieldOpts, _Rules, _Opts) ->
    {ok, undefined};
extract_generic(Name, Value, FieldOpts, Rules, Opts) ->
    case lists:member(recursive, FieldOpts) of
        true ->
            case lists:keyfind(type, 1, FieldOpts) of
                false ->
                    {error, {type_required, Name, FieldOpts}};
                {type, Type} ->
                    case decode1(Value, Type, Rules, Opts) of
                        {error, _} = E ->
                            E;
                        Decoded ->
                            apply_post_decode(Name, Decoded, FieldOpts)
                    end
            end;
        false ->
            apply_post_decode(Name, Value, FieldOpts)
    end.

extract_list(null, _FieldOpts, _Rules, _Opts) ->
    {ok, undefined};
extract_list(Value, FieldOpts, Rules, Opts) ->
    case proplists:get_value(type, FieldOpts) of
        undefined ->
            %% No target type for list element, it can be an attrlist
            %% or a primitive value
            L = lists:foldl(
                  fun(_, {error, _} = Acc) ->
                          Acc;
                     (V, Acc) when is_list(V) ->
                          case get_rec_type(V) of
                              undefined ->
                                  {error, no_record_type, V};
                              Type ->
                                  case decode1(V, Type, Rules, Opts) of
                                      {error, _} = E ->
                                          E;
                                      Decoded ->
                                          [Decoded | Acc]
                                  end
                          end;
                     (V, Acc) ->
                          [V | Acc]
                  end, [], Value),
            case L of
                {error, _} = E2 ->
                    E2;
                _ ->
                    {ok, lists:reverse(L)}
            end;
        Type ->
            L = lists:foldl(
                  fun(_, {error, _} = Acc) ->
                          Acc;
                     (V, Acc) ->
                          case decode1(V, Type, Rules, Opts) of
                              {error, _} = E3 ->
                                  E3;
                              Decoded ->
                                  [Decoded | Acc]
                          end
                  end, [], Value),
            case L of
                {error, _} = E4 ->
                    E4;
                _ ->
                    {ok, lists:reverse(L)}
            end
    end.

maybe_post_process({const, _Name, _Const}, Record, Value) ->
    {ok, Record, Value};
maybe_post_process({generic, _Name, _FieldOpts}, Record, Value) ->
    {ok, Record, Value};
maybe_post_process({virtual, Name, FieldOpts}, Record, Value) ->
    case lists:keyfind(post_decode, 1, FieldOpts) of
        false ->
            {error, {no_post_decode, Name, Value}};
        {post_decode, Fun} ->
            case safe_call_fun(Name, Record, [Record, Value], Fun) of
                {ok, _, NewRecord} ->
                    {ok, NewRecord};
                Error ->
                    Error
            end
    end;
maybe_post_process({Type, Name, FieldOpts}, Record, Value) ->
    case lists:keyfind(post_decode, 1, FieldOpts) of
        false ->
            case Type of
                generic ->
                    {error, {no_post_decode, Name, Value}};
                _ ->
                    {ok, Record, Value}
            end;
        {post_decode, Fun} ->
            safe_call_fun(Name, Record, [Value], Fun)
    end;
maybe_post_process(_, Record, Value) ->
    {ok, Record, Value}.

apply_post_decode(Name, Value, FieldOpts) ->
    case lists:keyfind(post_decode, 1, FieldOpts) of
        false ->
            {error, {no_post_decode, Name, FieldOpts}};
        {post_decode, {M, F}} ->
            try erlang:apply(M, F, [Value]) of
                Val ->
                    {ok, Val}
            catch
                E:R ->
                    {error, {Name, E, R, Value}}
            end
    end.

%% TODO sort the parameters
safe_call_fun(Name, Record, Args, {M, F}) ->
    try erlang:apply(M, F, Args) of
        Val ->
            {ok, Record, Val}
    catch
        E:R ->
            {error, {Name, E, R, Args}}
    end.

get_rec_type(JsxList) ->
    case lists:keyfind(<<"__rec">>, 1, JsxList) of
        false ->
            undefined;
        {_, Rec} ->
            binary_to_atom(Rec, utf8)
    end.

%% Get the default value from a field rule
default_value({skip, Opts}) ->
    lists:keyfind(default, 1, Opts);
default_value({Type, _, Opts}) when Type =:= atom orelse
                                    Type =:= binary orelse
                                    Type =:= boolean orelse
                                    Type =:= list orelse
                                    Type =:= number orelse
                                    Type =:= record orelse
                                    Type =:= string ->
    lists:keyfind(default, 1, Opts);
default_value(_) ->
    false.
