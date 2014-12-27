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

decode(AttrList, Opts) ->
    case lists:keyfind(<<"__rec">>, 1, AttrList) of
        {_, Rec} ->
            RecordName = list_to_atom(binary_to_list(Rec)),
            decode(AttrList, Opts, RecordName);
        false ->
            {error, no_record_name}
    end.

%% When we have proper target record name (3rd parameter as atom)
decode(AttrList, Opts, RecordName) ->
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
            [undefined | extract_fields(F, AttrList, Opts)];
        BareField ->
            Bf = if is_atom(BareField) ->
                        list_to_binary(atom_to_list(BareField));
                    true ->
                        list_to_binary(BareField)
                 end,
            case lists:keyfind(Bf, 1, AttrList) of
                false ->
                    {error, {no_value_for, Field}};
                {_, Value} ->
                    %% Extract value based on Field rule
                    Extracted = extract_value(Field, Value, Opts),
                    [Extracted | extract_fields(F, AttrList, Opts)]
            end
    end.

extract_value({list, _}, Value, Opts) ->
    [case V of
         _ when is_number(V) ->
             V;
         _ ->
             decode(V, Opts)
     end || V <- Value];
extract_value({list, _, Type}, Value, Opts) ->
    T = list_to_binary(atom_to_list(Type)),
    %% Add record meta info to each element of the list
    [decode([{<<"__rec">>, T} | V], Opts) || V <- Value];
extract_value({binary, _}, Value, _Opts) ->
    Value;
extract_value({string, _}, Value, _Opts) ->
    unicode:characters_to_list(Value);
extract_value({atom, _}, Value, _Opts) ->
    list_to_atom(binary_to_list(Value));
extract_value({proplist, _}, Value, _Opts) ->
    [{list_to_atom(binary_to_list(Prop)), Val}
     || {Prop, Val} <- Value, Prop =/= <<"__type">>];
extract_value({field_fun, _, _, {M, F}}, Value, _Opts) ->
    erlang:apply(M, F, [Value]);
extract_value({field_fun, _, _, DecFun}, Value, _Opts) ->
    DecFun(Value);
extract_value({rec_fun, _, _}, _Value, _Opts) ->
    undefined;
extract_value({const, _, _}, _Value, _Opts) ->
    undefined;
extract_value(_, Value, _Opts) ->
    Value.
