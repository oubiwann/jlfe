%% Copyright (c) 2008-2014 Robert Virding
%% Copyright (c) 2014 Duncan McGreggor
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% File    : lfe_eval.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang interpreter wrapper for LFE on Erjang.

%%% This is a real hack!

-module(jlfe_eval).

-export([expr/1,expr/2]).

%% Deprecated exports.
-export([eval/1,eval/2]).

-import(lfe_env, [new/0,add_vbinding/3,add_vbindings/2,get_vbinding/2,
          add_fbinding/4,add_fbindings/2,get_fbinding/3,
          add_ibinding/5,get_gbinding/3]).

-import(lists, [reverse/1,all/2,map/2,foldl/3,foldr/3]).
-import(orddict, [find/2,fetch/2,store/3,is_key/2]).

-compile({no_auto_import,[apply/3]}).        %For our apply/3 function
-deprecated([eval/1,eval/2]).

eval(E) -> expr(E).

eval(E, Env) -> expr(E, Env).

%% expr(Sexpr) -> Value.
%% expr(Sexpr, Env) -> Value.
%% Evaluate the sexpr, first expanding all macros.

expr(E) -> expr(E, lfe_env:new()).

expr(E, Env) ->
    %io:format("Made it to jlfe_eval:expr ...~n"),
    Exp = jlfe_macro:expand_expr_all(E, Env),
    case Exp of
    	no ->
            %io:format("Failed jlfe macro expansion.~n"),
            %io:format("Trying lfe macro expansion ...~n"),
    		lfe_eval:eval_expr(lfe_macro:expand_expr_all(E, Env), Exp);
    	_ ->
            %io:format("Successfully expanded jlfe macro; evaluating ...~n"),
    		lfe_eval:eval_expr(Exp, Env)
	end.

% expr(E, Env) ->
%     Exp = lfe_macro:expand_expr_all(E, Env),
%     case Exp of
%         no ->
%             io:format("Failed lfe macro expansion.~n"),
%             io:format("Trying jlfe macro expansion ...~n"),
%             lfe_eval:eval_expr(jlfe_macro:expand_expr_all(E, Env), Exp);
%         _ ->
%             lfe_eval:eval_expr(Exp, Env)
%     end.
