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
%%% @end
%%%-----------------------------------------------------------------------------
-module(ejson).

-export([
        to_json/2,
        from_json/2,
        json_props/1
    ]).

%%%============================================================================
%%% External API functions
%%%============================================================================

%%to_json_modules(Term, ModuleList) ->
%%    Opts = json_props(ModuleList),
%%    
%%    to_json(Term, Opts).
%%
%%to_json_module(Term, Module) ->
%%    %% Get -json attributes from module info
%%    Opts = json_props([Module]),
%%
%%    %% Call to_json with the Options we got
%%    to_json(Term, Opts).
%%
json_props(ModuleList) ->
    lists:foldl(
        fun(Module, Acc) ->
            Attrs = proplists:get_value(attributes, Module:module_info()),
            Opts = lists:flatten([V || {json, V} <- Attrs]),

            Opts ++ Acc
        end, [], ModuleList).

to_json(Term, Opts) ->
    Encoded = ejson_encode:encode(Term, Opts),
    jsx:encode(Encoded).

from_json(Binary, Opts) ->
    Decoded = jsx:decode(Binary),
    ejson_decode:decode(Decoded, Opts).
