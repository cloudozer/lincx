-module(linc_bootling).
-export([start/1]).

%% Entry point for start via bootling
start(SysConfFile) ->
	Apps = [crypto,asn1,public_key,ssh,compiler,syntax_tools,xmerl,mnesia,lager,linc],

	%% manually set apps params
	%% TODO: fix LING remote -config option
	[application:load(A) || A <- Apps],

	{ok, [Envs]} = file:consult(SysConfFile),
	lists:foreach(
		fun({App, Env}) ->
			[application:set_env(App, Par, Val) || {Par, Val} <- Env]
		end,
		Envs
	),

	[application:start(A) || A <- Apps].
