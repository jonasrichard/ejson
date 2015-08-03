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
        to_json/2,
        to_jsx/2,
        to_json_modules/2,
        to_jsx_modules/2,
        from_json/3,
        from_json_modules/3,
        json_props/1
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
    Opts = json_props(ModuleList),
    to_json(Term, Opts).

to_jsx_modules(Term, ModuleList) when is_list(ModuleList) ->
    Opts = json_props(ModuleList),
    to_jsx(Term, Opts).

-spec to_json(term(), rule()) -> {ok, jsx:jsx_term()} | {error, term()}.
to_json(Term, Opts) ->
    case to_jsx(Term, Opts) of
        {ok, Result} ->
            {ok, jsx:encode(Result)};
        {error, _} = Error ->
            Error
    end.

to_jsx(Term, Opts) ->
    ejson_encode:encode(Term, Opts).

from_json_modules(Binary, ModuleList, Record) ->
    Opts = json_props(ModuleList),
    from_json(Binary, Opts, Record).

from_json(Binary, Record, Opts) ->
    Decoded = jsx:decode(Binary),
    ejson_decode:decode(Decoded, Opts, Record).

%%%----------------------------------------------------------------------------
%%% @doc Get json attributes from the specified modules
%%% @end
%%%----------------------------------------------------------------------------
json_props(ModuleList) ->
    lists:foldl(
        fun(Module, Acc) ->
            Attrs = proplists:get_value(attributes, Module:module_info()),
            Opts = lists:flatten([V || {json, V} <- Attrs]),

            Opts ++ Acc
        end, [], ModuleList).
