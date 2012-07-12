-module(kadmin_gateways_controller, [Req, SessionID]).

-compile([{parse_transform, lager_transform}, export_all]).

-include("settings.hrl").

before_("login") -> ok;
before_(_) ->
    auth_lib:require_authentication(SessionID).

index('GET', [], {NavSpec, User}) ->
	[Host] = boss_db:find(kadmin_settings, [{key, 'equals', "kelly_host"}]),
	[Port] = boss_db:find(kadmin_settings, [{key, 'equals', "kelly_port"}]),
	lager:debug("Host: ~p, Port: ~p", [Host:value(), Port:value()]),
	Url = "http://10.10.0.155:8080/gateways",
	Result = httpc:request(get, {Url, []}, [], []),
	lager:debug("Getway request result: ~p", [Result]),
    {ok, [
		{your_login, User:login()},
		NavSpec]}.

%% update('POST', []) ->
%% 	Update = fun(Key) ->
%% 		Value = Req:post_param(Key),
%% 		lager:debug("Value: ~p", [Value]),
%% 		[Setting] = boss_db:find(kadmin_settings, [{key, 'equals', Key}]),
%% 		SettingNew = Setting:set(value, Value),
%% 		SettingNew:save()
%% 	end,
%% 	[Update(Key) || {Key, _} <- settings_spec()],
%% 	boss_flash:add(SessionID, notice, "Info", io_lib:format("Settings were updated", [])),
%%     {redirect, [{action, "index"}]}.
