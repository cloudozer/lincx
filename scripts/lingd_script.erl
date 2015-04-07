-module(lingd_script).
-export([start/5]).

% generate sys.config
% generate domain_config
start(Node, Domain, Img, Extra, Yml) ->
	{ok, [DefConfig]} = file:consult("/io/app/lincx/def.config"),
	%io:format("DefConfig: ~p\n", [DefConfig]),

	Queues = 
		lists:map(
			fun(Q) ->
				{queue, proplists:get_value("id", Q), [
					{min_rate, proplists:get_value("min", Q)},
					{max_rate, proplists:get_value("max", Q)}
				]}
			end,
			proplists:get_value("queues", Yml, [])
		),

	Ports1 =
		lists:map(
			fun(P) ->
				Id = proplists:get_value("id", P),
				{port, Id, [
					{interface,"eth" ++ integer_to_list(Id)},
					{type,vif}
				]}
			end,
			proplists:get_value("ports", Yml)
		),

	Ports2 =
		lists:map(
			fun(P) ->
				Q = proplists:get_value("queue", P, []),
				{port, proplists:get_value("id", P), {queues, lists:flatten([Q])}}
			end,
			proplists:get_value("ports", Yml)
		),

	Controllers =
		lists:map(
			fun(C) ->
				[Addr, Port] = string:tokens(C, ":"),
				{Addr, Addr, list_to_integer(Port), tcp}
			end,
			proplists:get_value("controllers", Yml)
		),

	[LAddr, LPort] = string:tokens(proplists:get_value("listen", Yml), ":"),

	LincConfig = [
		{of_config,disabled},
		{capable_switch_ports, Ports1},
		{capable_switch_queues, Queues},
		{logical_switches,
			[{switch,0,[
				{backend,linc_max},
				{controllers, Controllers},
				{controllers_listener, {LAddr, list_to_integer(LPort), tcp}},
				{queues_status, disabled},
				{ports, Ports2}
			]}]
		}
	],

	AdjConfig = adjust_sasl(Node, Domain, adjust_lager(Node, Domain, DefConfig)),

	SysConfigFile = "/io/" ++ Node ++ "/" ++ Domain ++ "/sys.config",
	ok = filelib:ensure_dir(SysConfigFile),
	{ok, SysConfigDev} = file:open(SysConfigFile, [write]),
	ok = io:format(SysConfigDev, "~p.\n", [[{linc, LincConfig} | AdjConfig]]),
	ok = file:close(SysConfigDev),

	% gen dom.config
	Memory = proplists:get_value("memory", Yml),
	Bridges = ["xenbr0"] ++ [proplists:get_value("bridge", P) || P <- proplists:get_value("ports", Yml)],
	Vif = "[" ++
			lists:foldl(
				fun
					(B, "") ->
						"'bridge=" ++ B ++ "'";
					(B, Acc) ->
						Acc ++ ",'bridge=" ++ B ++ "'"
				end,
				"",
				Bridges
			) ++
		"]",

	DomConfig =
		"name = \"" ++ Domain ++ "\"\n"
		"kernel = \"" ++ Img ++ "\"\n"
		"memory = " ++ integer_to_list(Memory) ++ "\n"
		"vif = " ++ Vif ++ "\n"
		"extra = \"" ++
			"-run linc_bootling start " ++ SysConfigFile ++
			" -home /lincx"
			" -pz "
				"/lincx/apps/linc/ebin "
				"/lincx/apps/linc_max/ebin "
				"/lincx/apps/linc_us3/ebin "
				"/lincx/apps/linc_us4/ebin "
				"/lincx/deps/eenum/ebin "
				"/lincx/deps/enetconf/ebin "
				"/lincx/deps/lager/ebin "
				"/lincx/deps/meck/ebin "
				"/lincx/deps/of_config/ebin "
				"/lincx/deps/of_protocol/ebin "
				"/lincx/deps/pkt/ebin " ++
			Extra,

	DomConfigFile = "/io/" ++ Node ++ "/" ++ Domain ++ "/dom.config",
	file:write_file(DomConfigFile, DomConfig),

	%io:format("Yml: ~p\n", [Yml]),
	%io:format("Dom: ~s\n", [DomConfig]),

	DomConfigFile.

adjust_sasl(Node, Domain, Config) ->
	Sasl = proplists:get_value(sasl, Config),
	{sasl_error_logger, {file, SaslErrorLoggerFile}} = lists:keyfind(sasl_error_logger, 1, Sasl),
	ErrorLoggerMfDir = proplists:get_value(error_logger_mf_dir, Sasl),

	NewErrorLoggerMfDir = adjust_log(Node, Domain, ErrorLoggerMfDir),
	NewSaslErrorLoggerFile = adjust_log(Node, Domain, SaslErrorLoggerFile),
	NewSasl1 = lists:keyreplace(sasl_error_logger, 1, Sasl, {sasl_error_logger, {file, NewSaslErrorLoggerFile}}),
	NewSasl2 = lists:keyreplace(error_logger_mf_dir, 1, NewSasl1, {error_logger_mf_dir, NewErrorLoggerMfDir}),
	lists:keyreplace(sasl, 1, Config, {sasl, NewSasl2}).

adjust_lager(Node, Domain, Config) ->
	Lager = proplists:get_value(lager, Config),
	CrashLog = proplists:get_value(crash_log, Lager),
	Handlers = proplists:get_value(handlers, Lager),

	NewFileBackend =
		lists:map(
			fun({File, Level, RotSize, RotDate, RotCount}) ->
				{adjust_log(Node, Domain, File), Level, RotSize, RotDate, RotCount}
			end,
			proplists:get_value(lager_file_backend, Handlers)
		),

	NewHandlers = lists:keyreplace(
		lager_file_backend, 1, Handlers, {lager_file_backend, NewFileBackend}
	),
	NewCrashLog = adjust_log(Node, Domain, CrashLog),
	NewLager1 = lists:keyreplace(handlers, 1, Lager, {handlers, NewHandlers}),
	NewLager2 = lists:keyreplace(crash_log, 1, NewLager1, {crash_log, NewCrashLog}),
	lists:keyreplace(lager, 1, Config, {lager, NewLager2}).

adjust_log(Node, Domain, Log) ->
	lists:flatten("/io/" ++ Node ++ "/" ++ Domain ++ "/" ++ Log).
