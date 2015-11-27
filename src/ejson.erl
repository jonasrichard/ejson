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
%%% ejson main module
%%%
%%% This module contains the main entry points of the library such as
%%% encoding, decoding. Those functions can result in jsx terms or binaries.
%%% @end
%%%-----------------------------------------------------------------------------
-module(ejson).

-export([
        to_json/3,
        to_jsx/3,
        to_json_modules/2,
        to_jsx_modules/2,
        from_json/3,
        from_json/4,
        from_json_modules/2,
        from_json_modules/3,
        json_rules/1,
        json_opts/1
    ]).

%%%----------------------------------------------------------------------------
%%% Types
%%%----------------------------------------------------------------------------

-type field_name()  :: atom() | string().

-type field_opt()   :: {default, term()} | {type, atom()}.
-type field_opts()  :: list(field_opt()).

-type type_sel()    :: atom | binary | list | record | string.
-type basic_rule()  :: field_name() |
                       {type_sel(), field_name()} |
                       {type_sel(), field_name(), field_opts()}.
-type rule()        :: list(basic_rule()).


%%%============================================================================
%%% External API functions
%%%============================================================================

-spec to_json_modules(term(), [module()]) ->
        {ok, binary()} |
        {error, {duplicate_records, term()}} |
        {error, {duplicate_fields, term()}}.
to_json_modules(Term, ModuleList) ->
    Rules = json_rules(ModuleList),
    Opts = json_opts(ModuleList),
    to_json(Term, Rules, Opts).

to_jsx_modules(Term, ModuleList) when is_list(ModuleList) ->
    Rules = json_rules(ModuleList),
    Opts = json_opts(ModuleList),
    to_jsx(Term, Rules, Opts).

-spec to_json(term(), rule(), list()) -> {ok, jsx:jsx_term()} | {error, term()}.
to_json(Term, Rules, Opts) ->
    case to_jsx(Term, Rules, Opts) of
        {ok, Result} ->
            {ok, jsx:encode(Result)};
        {error, _} = Error ->
            Error
    end.

to_jsx(Term, Rules, Opts) ->
    ejson_encode:encode(Term, Rules, Opts).

from_json_modules(Binary, ModuleList) ->
    Rules = json_rules(ModuleList),
    Opts = json_opts(ModuleList),
    from_json(Binary, Rules, Opts).

from_json_modules(Binary, ModuleList, Record) ->
    Opts = json_rules(ModuleList),
    from_json(Binary, Opts, Record).

from_json(Binary, Rules, Opts) ->
    Decoded = jsx:decode(Binary),
    case lists:keyfind(<<"__rec">>, 1, Decoded) of
        {<<"__rec">>, TypeBin} ->
            Record = binary_to_atom(TypeBin, utf8),
            ejson_decode:decode(Decoded, Record, Rules, Opts);
        false ->
            {error, no_root_record_type}
    end.

from_json(Binary, Record, Rules, Opts) ->
    Decoded = jsx:decode(Binary),
    ejson_decode:decode(Decoded, Record, Rules, Opts).

%%%----------------------------------------------------------------------------
%%% @doc Get json attributes from the specified modules
%%% @end
%%%----------------------------------------------------------------------------
json_rules(ModuleList) ->
    lists:foldl(
        fun(Module, Acc) ->
            extract_attrs(Module, json) ++ Acc
        end, [], ModuleList).

json_opts(ModuleList) ->
    lists:foldl(
      fun(Module, Acc) ->
          extract_attrs(Module, json_opt) ++ Acc
      end, [], ModuleList).

extract_attrs(Module, Attr) ->
    [V || {A, [V]} <- proplists:get_value(attributes, Module:module_info()),
          A =:= Attr].
