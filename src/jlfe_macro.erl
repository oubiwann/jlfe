%% Copyright (c) 2008-2013 Robert Virding
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

%% File    : lfe_macro.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang macro expander wrapper for LFE on Erjang.

%% Expand macros and record definitions (into macros), also handles
%% quasiquote/backquote in an R6RS compatible way.

-module(jlfe_macro).

-compile(export_all).

%% These work on individual expressions.
-export([expand_expr_all/2]).



%% -compile([export_all]).

-import(lfe_env, [new/0,add_fbinding/4,is_fbound/3,
          add_mbinding/3,is_mbound/2,get_mbinding/2]).

-import(lfe_lib, [is_symb_list/1,is_proper_list/1]).

-import(lists, [any/2,all/2,map/2,foldl/3,foldr/3,mapfoldl/3,
        reverse/1,reverse/2,member/2,concat/1]).
-import(orddict, [find/2,store/3]).
-import(ordsets, [add_element/2,is_element/2]).

-include("deps/lfe/src/lfe_macro.hrl").


expand_expr_all(F, Env) ->
    {Ef,_} = exp_form(F, Env, #mac{expand=true}),
    Ef.

%% exp_form(Form, Env, State) -> {Form,State}.
%%  Completely expand a form using expansions in Env and pre-defined
%%  macros.  N.B. builtin core forms cannot be overidden and are
%%  handled here first. The core forms also are particular about how
%%  their bodies are to be expanded.

%% Known Core forms which cannot be overidden.
exp_form([quote,_]=Q, _, St) -> {Q,St};
exp_form([cons,H0,T0], Env, St0) ->
    {H1,St1} = exp_form(H0, Env, St0),
    {T1,St2} = exp_form(T0, Env, St1),
    {[cons,H1,T1],St2};
exp_form([car,E0], Env, St0) ->            %Catch these to prevent
    {E1,St1} = exp_form(E0, Env, St0),        %redefining them
    {[car,E1],St1};
exp_form([cdr,E0], Env, St0) ->
    {E1,St1} = exp_form(E0, Env, St0),
    {[cdr,E1],St1};
exp_form([list|As0], Env, St0) ->
    {As1,St1} = lfe_macro:exp_tail(As0, Env, St0),
    {[list|As1],St1};
exp_form([tuple|As0], Env, St0) ->
    {As1,St1} = lfe_macro:exp_tail(As0, Env, St0),
    {[tuple|As1],St1};
exp_form([binary|As0], Env, St0) ->
    {As1,St1} = lfe_macro:exp_tail(As0, Env, St0),
    {[binary|As1],St1};
exp_form(['lambda',Head|B0], Env, St0) ->
    {B1,St1} = lfe_macro:exp_tail(B0, Env, St0),
    {['lambda',Head|B1],St1};
exp_form(['match-lambda'|B0], Env, St0) ->
    {B1,St1} = lfe_macro:exp_ml_clauses(B0, Env, St0),
    {['match-lambda'|B1],St1};
exp_form(['let',Vbs0|B0], Env, St0) ->
    %% We don't really have to syntax check very strongly here so we
    %% can use normal clause expansion. Lint will catch errors.
    {Vbs1,St1} = lfe_macro:exp_clauses(Vbs0, Env, St0),
    {B1,St2} = lfe_macro:exp_tail(B0, Env, St1),
    {['let',Vbs1|B1],St2};
exp_form(['let-function',Fbs|B], Env, St) ->
    lfe_macro:exp_let_function(Fbs, B, Env, St);
exp_form(['letrec-function',Fbs|B], Env, St) ->
    lfe_macro:exp_letrec_function(Fbs, B, Env, St);
exp_form(['let-macro',Mbs|B], Env, St) ->
    lfe_macro:exp_let_macro(Mbs, B, Env, St);
exp_form(['progn'|B0], Env, St0)->
    {B1,St1} = lfe_macro:exp_tail(B0, Env, St0),
    {['progn'|B1],St1};
exp_form(['if'|B0], Env, St0) ->
    {B1,St1} = lfe_macro:exp_tail(B0, Env, St0),
    {['if'|B1],St1};
exp_form(['case',E0|Cls0], Env, St0) ->
    {E1,St1} = exp_form(E0, Env, St0),
    {Cls1,St2} = lfe_macro:exp_clauses(Cls0, Env, St1),
    {['case',E1|Cls1],St2};
exp_form(['receive'|Cls0], Env, St0) ->
    {Cls1,St1} = lfe_macro:exp_clauses(Cls0, Env, St0),
    {['receive'|Cls1],St1};
exp_form(['catch'|B0], Env, St0) ->
    {B1,St1} = lfe_macro:exp_tail(B0, Env, St0),
    {['catch'|B1],St1};
exp_form(['try',E|B], Env, St) ->
    lfe_macro:exp_try(E, B, Env, St);
exp_form(['funcall'|As0], Env, St0) ->
    {As1,St1} = lfe_macro:exp_tail(As0, Env, St0),
    {['funcall'|As1],St1};
exp_form(['call'|As0], Env, St0) ->
    {As1,St1} = lfe_macro:exp_tail(As0, Env, St0),
    {['call'|As1],St1};
exp_form(['define-function',Head|B0], Env, St0) ->
    %% Needs to be handled specially to protect Head.
    {B1,St1} = lfe_macro:exp_tail(B0, Env, St0),
    {['define-function',Head|B1],St1};
%% Now the case where we can have macros.
exp_form([Fun|_]=Call, Env, St0) when is_atom(Fun) ->
    %% Expand top macro as much as possible.
    case exp_macro(Call, Env, St0) of
    {yes,Exp,St1} -> exp_form(Exp, Env, St1);
    no -> lfe_macro:exp_tail(Call, Env, St0)
    end;
exp_form([_|_]=Call, Env, St) -> lfe_macro:exp_tail(Call, Env, St);
exp_form(Tup, _, St) when is_tuple(Tup) ->
    %% Should we expand this? We assume implicit quote here.
    {Tup,St};
%% Everything else is atomic.
exp_form(F, _, St) -> {F,St}.            %Atomic

%% XXX this module doesn't define expand_expr_1 like lfe_eval.erl does; that
%% function is used by lfe_lib:macroexpand-1; as such, calling macroexpand
%% from the shell won't expand any jlfe-specific macros right now ...

exp_macro([Name|_]=Call, Env, St) ->
    case lfe_lib:is_core_form(Name) of
    true -> no;                %Never expand core forms
    false ->
        case get_mbinding(Name, Env) of
        {yes,Def} ->
            %% User macro bindings.
            exp_userdef_macro(Call, Def, Env, St);
        no ->
            %% Default macro bindings.
            %io:format("Preparing to expand predef macro ...~n"),
            exp_predef_macro(Call, Env, St)
        end
    end.

exp_userdef_macro([Mac|Args], Def0, Env, St0) ->
    %%lfe_io:format("udef: ~p\n", [[Mac|Args]]),
    %%lfe_io:format("macro: ~p\n", [Def0]),
    try
    {Def1,St1} = exp_form(Def0, Env, St0),    %Expand definition
    Exp = lfe_eval:apply(Def1, [Args,Env], Env),
    {yes,Exp,St1}
    catch
    error:Error ->
        Stack = erlang:get_stacktrace(),
        erlang:error({expand_macro,[Mac|Args],{Error,Stack}})
    end.

exp_predef_macro(Call, Env, St) ->
    %%lfe_io:format("pdef: ~p\n", [Call]),
    try
        Exp = exp_predef(Call, Env, St),
        case Exp of
            no -> lfe_macro:exp_predef(Call, Env, St);
            _ -> Exp
        end
    catch
    error:Error ->
        Stack = erlang:get_stacktrace(),
        erlang:error({expand_macro,Call,{Error,Stack}})
    end.

%% check for Java calls
% exp_predef(['obj',M,F|As], _, St) ->
%     {yes,['call',?Q(M),?Q(F)|As], St};
exp_predef([DotFun|As], _, St) when is_atom(DotFun) ->
    %io:format("Made it to jlfe_macro:exp_predef ...~n"),
    ListDotFun = atom_to_list(DotFun),
    case lists:nth(1, ListDotFun) of
    46 ->           % ASCII for the period or "."
        %io:format("Made it to a good outer match in jlfe_macro:exp_predef ...~n"),
        case string:tokens(lists:nthtail(1, ListDotFun), ":") of
        [M,F] ->
            %io:format("Made it to a good inner match in jlfe_macro:exp_predef ...~n`"),
            % to pass mod, func, and list of args
            %Newargs = [?Q([list_to_atom(M),list_to_atom(F),As])],
            % to pass everything as a single arg:
            Newargs = [?Q([list_to_atom(M),list_to_atom(F)] ++ As)],
            %io:format("New args: ~p~n", [Newargs]),
            {yes,[call,?Q(jlfe_java),?Q(call)|Newargs],St};
        %% The only case we expect a dot form to not have a ":" separator is
        %% when being used as a constructor, so we'll append a "new" here.
        [M] ->
            Newargs = [?Q([list_to_atom(M),new] ++ As)],
            io:format("New args: ~p~n", [Newargs]),
            {yes,[call,?Q(jlfe_java),?Q(call)|Newargs],St};
        _ ->
        io:format("Made it to a bad inner match in jlfe_macro:exp_predef ...~n"),
        no
        end;
    _ ->
        %io:format("Made it to a bad outer match in jlfe_macro:exp_predef ...~n"),
        no
    end;
% exp_predef([Fun|As], _, St) when is_atom(Fun) ->
%     case string:tokens(atom_to_list(Fun), ":") of
%         [M,F] ->
%             {yes,[call,?Q(list_to_atom(M)),?Q(list_to_atom(F))|As],St};
%         _ -> no                                 %This will also catch a:b:c
%     end;
% This was not a call to a predefined macro.
exp_predef(_, _, _) -> no.
