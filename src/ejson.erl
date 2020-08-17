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
        json_opts/1,
        json_modules/1
    ]).

%%%----------------------------------------------------------------------------
%%% Types
%%%----------------------------------------------------------------------------

-type field_name()  :: atom() | string().

-type field_def()   :: {default, term()}.
%-type field_enc()   :: {pre_encode, fun((term(), term()) -> term())} |
%                       {post_decode, fun((term()) -> term())}.
-type field_opt()   :: field_def() | {type, atom()} | recursive.
-type field_opts()  :: list(field_opt()).

-type type_sel()    :: atom | binary | boolean | list | number | record | string.
-type basic_rule()  :: field_name() |
                       {type_sel(), field_name()} |
                       {type_sel(), field_name(), field_opts()} |
                       {skip, list(field_def())} |
                       {const, field_name(), term()} |
                       {generic, field_name(), field_opts()}.
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
    Rules = json_rules(ModuleList),
    Opts = json_opts(ModuleList),
    from_json(Binary, Record, Rules, Opts).

from_json(Binary, Rules, Opts) ->
    Decoded = jsx:decode(Binary, [{return_maps, false}]),
    %% We don't know what can be in the result, so detect it!
    %% It can be primitive value, primitive value list, or list of records
    %% or simple just a record.
    decode_value(Decoded, Rules, Opts).

from_json(Binary, Record, Rules, Opts) ->
    Decoded = jsx:decode(Binary, [{return_maps, false}]),
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

%% Get all rules and opts from a module, taking care of json_include attributes.
%% For dynamic rule collection {module, Module} field opts
json_modules(Module) ->
    Modules = [Module | extract_attrs(Module, json_include)],
    {json_rules(Modules), json_opts(Modules)}.

extract_attrs(Module, Attr) ->
    [V || {A, [V]} <- proplists:get_value(attributes, Module:module_info()),
          A =:= Attr].

%% Detect the target value: number, null, list, complex_list or record
jsx_detect(L) when is_list(L) ->
    Primitives = lists:all(
                   fun(Elem) ->
                           is_number(Elem) orelse is_atom(Elem)
                   end, L),
    case Primitives of
        true ->
            list;
        false ->
            Fields = lists:all(
                       fun({Name, _Value}) ->
                               is_binary(Name);
                          (_) ->
                               false
                       end, L),
            case Fields of
                true ->
                    record;
                false ->
                    complex_list
            end
    end;
jsx_detect(N) when is_number(N) ->
    number;
jsx_detect(null) ->
    null.

%% Decode the value according to the detection
decode_value(Decoded, Rules, Opts) ->
    case jsx_detect(Decoded) of
        number ->
            {ok, Decoded};
        null ->
            {ok, undefined};
        record ->
            case lists:keyfind(<<"__rec">>, 1, Decoded) of
                {<<"__rec">>, TypeBin} ->
                    Record = binary_to_atom(TypeBin, utf8),
                    ejson_decode:decode(Decoded, Record, Rules, Opts);
                false ->
                    {error, no_root_record_type}
            end;
        list ->
            {ok, Decoded};
        complex_list ->
            complex_list(Decoded, Rules, Opts)
    end.

%% Decode complex list recursively
complex_list(List, Rules, Opts) ->
    Result = lists:foldl(
               fun(_, {error, _} = E) ->
                       E;
                  (Elem, Acc) ->
                       case decode_value(Elem, Rules, Opts) of
                           {ok, Value} ->
                               [Value | Acc];
                           {error, _} = E ->
                               E
                       end
               end, [], List),
    case Result of
        {error, _} = E2 ->
            E2;
        _ ->
            {ok, Result}
    end.
