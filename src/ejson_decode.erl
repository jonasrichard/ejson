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

-export([decode/2]).

decode(AttrList, Opts) ->
    case lists:keyfind(<<"__rec">>, 1, AttrList) of
        {_, Rec} ->
            RecordName = list_to_atom(binary_to_list(Rec)),
            case ejson_util:get_fields(RecordName, Opts) of
                {error, _} = Error ->
                    Error;
                Fields ->
                    Values = extract_fields(Fields, AttrList, Opts),
                    list_to_tuple([RecordName | Values])
            end;
        false ->
            {error, no_record_name}
    end.

extract_fields([], _, _) ->
    [];
extract_fields([Field | F], AttrList, Opts) ->
    Bf = list_to_binary(atom_to_list(Field)),
    case lists:keyfind(Bf, 1, AttrList) of
        false ->
            {error, {no_value_for, Field}};
        {_, Value} ->
            [Value | extract_fields(F, AttrList, Opts)]
    end.
