-module(kadmin_01_news).

-compile([{parse_transform, lager_transform}]).

-include("kadmin_user.hrl").
-include("settings.hrl").

-export([init/0, stop/1]).

% This script is first executed at server startup and should
% return a list of WatchIDs that should be cancelled in the stop
% function below (stop is executed if the script is ever reloaded).
init() ->
	lager:debug("initialization"),

	%% starts necessary applications
	%% ./priv/rebar/boss_rebar.erl -> start_dev_cmd
	ok = application:start(compiler),
	ok = application:start(syntax_tools),
	ok = application:start(lager),
	ok = application:start(inets),
	ok = application:start(jsx),

	%% create admin if does not exist
	create_admin(),
	create_users_for_tests(100),

	initialize_settings(),
    {ok, []}.

stop(ListOfWatchIDs) ->
    lists:map(fun boss_news:cancel_watch/1, ListOfWatchIDs).

initialize_settings() ->
	InitSettingFun = fun(Key, Value) ->
		case boss_db:find(kadmin_settings, [{key, 'equals', Key}]) of
			[] ->
				Setting = kadmin_settings:new(id, Key, Value),
				Setting:save(),
				lager:info("Setting: ~p was set to ~p", [Key, Value]);
			{error, Reason} -> {error, Reason};
			_ -> ok
		end
	end,
	[InitSettingFun(Key, Value) || {Key, Value} <- ?settings_spec].

create_admin() ->
	case boss_db:find(kadmin_user, [{login, 'equals', "admin"}]) of
		[] ->
			Login = "admin",
			FullName = "Anton Sizov",
			PassHash = erlang:md5("admin"),
			State = ?KU_ACTIVE,
			ContactData = "a.sysoff@gmail.com",
			Admin = kadmin_user:new(id, Login, FullName, PassHash, State, ContactData),
			Admin:save(),
			lager:debug("Superadministrator was created");
		Any ->
			lager:debug("Superadministrator is existing")
	end.

create_users_for_tests(0) ->
	ok;
create_users_for_tests(N) ->
	Login = "test_" ++ integer_to_list(N),
	FullName = "Test User " ++ integer_to_list(N),
	PassHash = erlang:md5("test" ++ integer_to_list(N)),
	State = ?KU_ACTIVE,
	ContactData = "testuser"  ++ integer_to_list(N) ++ "@gmail.com",
	User = kadmin_user:new(id, Login, FullName, PassHash, State, ContactData),
	lager:debug("User: ~p", [User]),
	User:save(),
	lager:debug("Test user ~p was created", [Login]),
	create_users_for_tests(N-1).

%%%%%%%%%%% Ideas
%    boss_news:watch("user-42.*",
%        fun
%            (updated, {Donald, 'location', OldLocation, NewLocation}) ->
%                ;
%            (updated, {Donald, 'email_address', OldEmail, NewEmail})
%        end),
%
%    boss_news:watch("user-*.status",
%        fun(updated, {User, 'status', OldStatus, NewStatus}) ->
%                Followers = User:followers(),
%                lists:map(fun(Follower) ->
%                            Follower:notify_status_update(User, NewStatus)
%                    end, Followers)
%        end),
%
%    boss_news:watch("users",
%        fun
%            (created, NewUser) ->
%                boss_mail:send(?WEBSITE_EMAIL_ADDRESS,
%                    ?ADMINISTRATOR_EMAIL_ADDRESS,
%                    "New account!",
%                    "~p just created an account!~n",
%                    [NewUser:name()]);
%            (deleted, OldUser) ->
%                ok
%        end),
%
%    boss_news:watch("forum_replies",
%        fun
%            (created, Reply) ->
%                OrignalPost = Reply:original_post(),
%                OriginalAuthor = OriginalPost:author(),
%                case OriginalAuthor:is_online() of
%                    true ->
%                        boss_mq:push(OriginalAuthor:comet_channel(), <<"Someone replied!">>);
%                    false ->
%                        case OriginalAuthor:likes_email() of
%                            true ->
%                                boss_mail:send("website@blahblahblah",
%                                    OriginalAuthor:email_address(),
%                                    "Someone replied!"
%                                    "~p has replied to your post on ~p~n",
%                                    [(Reply:author()):name(), OriginalPost:title()]);
%                            false ->
%                                ok
%                        end
%                end;
%            (_, _) -> ok
%        end),
%
%    boss_news:watch("forum_categories",
%        fun
%            (created, NewCategory) ->
%                boss_mail:send(?WEBSITE_EMAIL_ADDRESS,
%                    ?ADMINISTRATOR_EMAIL_ADDRESS,
%                    "New category: "++NewCategory:name(),
%                    "~p has created a new forum category called \"~p\"~n",
%                    [(NewCategory:created_by()):name(), NewCategory:name()]);
%            (_, _) -> ok
%        end),
%
%    boss_news:watch("forum_category-*.is_deleted",
%        fun
%            (updated, {ForumCategory, 'is_deleted', false, true}) ->
%                ;
%            (updated, {ForumCategory, 'is_deleted', true, false}) ->
%        end).

% Invoking the API directly:
%boss_news:deleted("person-42", OldAttrs),
%boss_news:updated("person-42", OldAttrs, NewAttrs),
%boss_news:created("person-42", NewAttrs)

% Invoking the API via HTTP (with the admin application installed):
% POST /admin/news_api/deleted/person-42
% old[status] = something

% POST /admin/news_api/updated/person-42
% old[status] = blah
% new[status] = barf

% POST /admin/news_api/created/person-42
% new[status] = something
