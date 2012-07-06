-module(kadmin_settings, [Id, Key, Value]).

-compile([{parse_transform, lager_transform}, export_all]).

before_create() ->
	Result = boss_db:find(kadmin_settings, [{key, 'equals', Key}]),
	lager:debug("Result: ~p", [Result]),
	case Result of
		[] -> ok;
		{error, Reason} -> {error, Reason};
		_ -> {error, key_in_use}
	end.
