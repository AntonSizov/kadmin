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
    {render_other, [{action, "edit_form"}]};
create('POST', []) ->
	Login = Req:post_param("login"),
	FullName = Req:post_param("full_name"),
	PassHash = erlang:md5(Req:post_param("password")),
	State = Req:post_param("state"),
	ContactData = Req:post_param("contact_data"),
	User = kadmin_user:new(id, Login, FullName, PassHash, State, ContactData),
	User:save(),
	boss_flash:add(SessionID, notice, "Info", io_lib:format("New user ~p sucessfully created", [Login])),
	{redirect, [{action, "update"}, {login, Login}]}.

index('GET', []) ->
	{redirect, [{action, "index"}, {page, 1}]};
index('GET', [Page]) ->
	IPage = list_to_integer(Page),
	lager:debug("P: ~p", [Page]),
	Limit = 10,
	Skip = Limit * (IPage - 1),
	Users = boss_db:find(kadmin_user, [], Limit, Skip),
	UsersNumber = boss_db:count(kadmin_user),
	PagesNumber = trunc(
		case (UsersNumber rem Limit) of
			0 ->	UsersNumber/Limit;
			_ -> UsersNumber div Limit + 1
		end ),
	lager:debug("PagesNumber: ~p", [PagesNumber]),
	UserList = [proplists:delete(pass_hash, U:attributes()) || U <- Users],
	lager:debug("User list: ~p", [UserList]),
    {ok, [
		{total_rows, boss_db:count(kadmin_user)},
		{users, UserList},
		{page_numbers, lists:seq(1, PagesNumber)},
		{active_page, IPage}
	   	]}.

update('GET', [Login]) ->
	case boss_db:find(kadmin_user, [{login, 'equals', Login}]) of
		[] ->
			boss_flash:add(SessionID, error, "Error", io_lib:format("User ~p not found", [Login])),
			{redirect, [{action, "index"}]};
		[FoundUser] ->
		    {render_other, [{action, "edit_form"}], [
				{user, FoundUser}
			   	]}
	end;
update('POST', [Login]) ->
	case boss_db:find(kadmin_user, [{login, 'equals', Login}]) of
		[] ->
			boss_flash:add(SessionID, error, "Error", io_lib:format("User ~p not found", [Login])),
			{redirect, [{action, "index"}]};
		[FoundUser] ->
   			NewLogin = Req:post_param("login"),
			FullName = Req:post_param("full_name"),
			PassHash = erlang:md5(Req:post_param("password")),
			State = Req:post_param("state"),
			ContactData = Req:post_param("contact_data"),
			UpdatedUser = FoundUser:set([{login, NewLogin}, {full_name, FullName}, {pass_hash, PassHash}, {state, State}, {contact_data, ContactData}]),
			UpdatedUser:save(),
			boss_flash:add(SessionID, notice, "Info", io_lib:format("User ~p sucessfully updated", [Login])),
 			{redirect, [{action, "update"}, {login, NewLogin}]}
	end.


delete('GET', ["admin"]) ->
	boss_flash:add(SessionID, error, "Error", "You cann't remove system user account \"admin\""),
	{redirect, [{action, "index"}]};
delete('GET', [Login]) ->
	case boss_db:find(kadmin_user, [{login, 'equals', Login}]) of
		[] ->
			lager:debug("User not found"),
			boss_flash:add(SessionID, notice, "Action result", io_lib:format("User ~p not found", [Login])),
			{redirect, [{action, "index"}]};
		[User] ->
			ok = boss_db:delete(User:id()),
			boss_flash:add(SessionID, notice, "Action result", io_lib:format("User ~p deleted", [Login])),
			{redirect, [{action, "index"}]}
	end.
