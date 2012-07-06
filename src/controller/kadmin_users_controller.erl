-module(kadmin_users_controller, [Req, SessionID]).

-compile([{parse_transform, lager_transform}, export_all]).

before_("login") -> ok;
before_(_) ->
    auth_lib:require_authentication(SessionID).


login('GET', []) ->
    ok;
login('POST', []) ->
    Login = Req:post_param("login"),
    PasswordHash = erlang:md5(Req:post_param("password")),
    case boss_db:find(kadmin_user, [{login, 'equals', Login}, {pass_hash, 'equals', PasswordHash}]) of
        {error, Reason} ->
            boss_flash:add(SessionID, error, "Error", io_lib:format("~p", [Reason])),
            ok;
        [] ->
            boss_flash:add(SessionID, error, "Error", "User is not authenticated."),
            {ok, [{login, Login}]};
        [User] ->
            boss_session:set_session_data(SessionID, principal, User:id()),
            {redirect, "/"}
    end.

logout('GET', []) ->
    boss_session:delete_session(SessionID),
    {redirect, "/login"}.

create('GET', []) ->
    ok.
index('GET', [], {NavSpec, User}) ->
	Limit = 10,
	Users = boss_db:find(kadmin_user, [], Limit),
	UsersNumber = boss_db:count(kadmin_user),

	PagesNumber =
		case (UsersNumber rem Limit) of
			0 ->	UsersNumber/Limit;
			_ -> UsersNumber div Limit + 1
		end,
	lager:debug("PagesNumber: ~p", [PagesNumber]),
	UserList = [proplists:delete(pass_hash, User:attributes()) || User <- Users],
	lager:debug("User list: ~p", [UserList]),
    {ok, [
		{your_login, User:login()},
		NavSpec,
		{total_rows, boss_db:count(kadmin_user)},
		{users, UserList},
		{page_numbers, lists:seq(1, PagesNumber)}
	   	]};

index('GET', [P], {NavSpec, User}) ->
	Page = list_to_integer(P),
	lager:debug("P: ~p", [P]),
	Limit = 10,
	Skip = Limit * (Page - 1),
	Users = boss_db:find(kadmin_user, [], Limit, Skip),
	UsersNumber = boss_db:count(kadmin_user),
	PagesNumber =
		case (UsersNumber rem Limit) of
			0 ->	UsersNumber/Limit;
			_ -> UsersNumber div Limit + 1
		end,
	lager:debug("PagesNumber: ~p", [PagesNumber]),
	UserList = [proplists:delete(pass_hash, User:attributes()) || User <- Users],
	lager:debug("User list: ~p", [UserList]),
    {ok, [
		{your_login, User:login()},
		NavSpec,
		{total_rows, boss_db:count(kadmin_user)},
		{users, UserList},
		{page_numbers, lists:seq(1, PagesNumber)}
	   	]}.

get_user('GET', []) ->
    ok.
update('POST', []) ->
    ok.
delete('GET', [UserId], {NavSpec, User}) ->
	lager:debug("UserId: ~p", [UserId]),
	Result = boss_db:delete(UserId),
	lager:debug("boss db delete result: ~p", [Result]),
	{redirect, "/users/index"}.
