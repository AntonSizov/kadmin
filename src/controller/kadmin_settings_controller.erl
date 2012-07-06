-module(kadmin_settings_controller, [Req, SessionID]).

-compile([{parse_transform, lager_transform}, export_all]).

-include("settings.hrl").

before_("login") -> ok;
before_(_) ->
    auth_lib:require_authentication(SessionID).

index('GET', [], {NavSpec, User}) ->
	Settings = boss_db:find(kadmin_settings, []),
	lager:debug("Settings: ~p", [Settings]),
    {ok, [
		{your_login, User:login()},
		NavSpec] ++ [{Setting:key(),Setting:value()} || Setting <- Settings]}.

update('POST', []) ->
	Update = fun(Key) ->
		Value = Req:post_param(Key),
		lager:debug("Value: ~p", [Value]),
		[Setting] = boss_db:find(kadmin_settings, [{key, 'equals', Key}]),
		SettingNew = Setting:set(value, Value),
		SettingNew:save()
	end,
	[Update(Key) || {Key, _} <- settings_spec()],
    {redirect, "/settings/index"}.
