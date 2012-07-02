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
	UserList = [proplists:delete(pass_hash, User:attributes()) || User <- Users],
	lager:debug("User list: ~p", [UserList]),
    {ok, [
		{your_login, User:login()},
		NavSpec,
		{total_rows, boss_db:count(kadmin_user)},
		{users, UserList}
	   	]}.

get_user('GET', []) ->
    ok.
update('POST', []) ->
    ok.
delete('POST', []) ->
    ok.
