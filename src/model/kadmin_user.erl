-module(kadmin_user, [Id, Login, FullName, PassHash, State, ContactData]).

-compile([{parse_transform, lager_transform}, export_all]).

validation_tests() ->
	[{fun() -> length(Login) > 3 end,
	"Login must be more than from 3 to 15 characters!"},
	{fun() -> length(Login) =< 15 end,
	"Login must be less than from 3 to 15 characters!"}].

before_create() ->
	Result = boss_db:find(kadmin_user, [{login, 'equals', Login}]),
	lager:debug("Result: ~p", [Result]),
	case Result of
		[] -> ok;
		{error, Reason} -> {error, Reason};
		_ -> {error, login_in_use}
	end.
