%% EXPERIMENTAL interface to zwizwa/erl_tools redo.
%%
%% What you need to know: this is used to generate the "boring"
%% Makefiles for anyone that does not need the redo functionality.
%% That makes it easier to integrate uc_tools into an existing build
%% system.  The API of this file is not stable yet, but API of the
%% Makefile is kept backwards compatible.


%% For those that are interested what this is about, see src/redo.erl
%% in http://github.com/zwizwa/erl_tools which is based on djb's redo
%% idea and apenwarr's writings and implementation.
-module(uc_tools_gdb_do).
-export([do/0]).

%% Some conventions:
%%
%% - Files use a dotted multi-extension format.  We keep using that
%%   idea but map it to lists, with the most general type extension at
%%   the head of the list i.e. reversed wrt the filename.
%%
%% - Quantifiers are encoded as expressed as {Quant,Arg}.  The set we
%%   quantify against is files stored in the local directory or any
%%   explicit subdirectories.
%%
%% - File names are assumed to be representable as Erlang atoms.
%%

do() ->
    #{ inputs  => {forall,[c]},
       outputs => [{o,f103,csp}],
       update  => fun update/1
     }.

%% C files are inputs.  redo:file_changed/3 checks if the file changed
%% since last run.
update({c,BaseName}=Target) ->
    fun(RedoPid) ->
            log:info("c: ~p~n", [BaseName]),
            File = fmt("~p.c", [BaseName]),
            {redo:file_changed(RedoPid, Target, File), []}
    end;

%% All the other rules follow the Deps,Script mechanism where a target
%% is mapped to a set of dependencies and a script that generates the
%% target.  The function here implements that wrapper...
update(Target) ->
    {Deps, Script} = deps(Target),
    fun(RedoPid) ->
            Cmds = script(Script),
            redo:need(RedoPid, Deps),
            log:info("Cmds = ~s~n", [Cmds]),
            Changed = true,
            {Changed, Deps}
    end.

%% ... and the code below implements the depdency list ...
deps(Target) ->
    case Target of
        {o,Arch,BaseName} ->
            {[{c,BaseName}],
             {gcc_o, Arch, BaseName}};
        _ ->
            throw({?MODULE,no_update_rule_for,Target})
    end.

%% ... and a translation from script spec to shell code.
script(Spec) ->
    case Spec of
        {gcc_o,Arch,BN} -> fmt("./gcc.~p.sh -o ~p.o -c ~p.c~n", [Arch, BN, BN])
    end.

fmt(F,A) ->    
    tools:format(F,A).
