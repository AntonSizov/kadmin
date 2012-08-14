-module(kadmin_providers_controller, [Req, SessionID]).

-compile([{parse_transform, lager_transform}, export_all]).

-include("settings.hrl").

before_("login") -> ok;
before_(_) ->
    auth_lib:require_authentication(SessionID).

index('GET', [], {NavSpec, User}) ->
	{ok, Providers} = k_lib:get_providers(),
	Total = length(Providers),
    {ok, [
		{providers, Providers},
		{your_login, User:login()},
		{total_rows, Total},
		{active_page, 1},
		{page_numbers, [1]},
		NavSpec]}.

delete('GET', [ID], {_NavSpec, _}) ->
	boss_flash:add(SessionID, notice, "Action result", io_lib:format("Provider ~p was successfully deleted", [ID])),
	ok = k_lib:del_provider(ID),
	{redirect, [{action, "index"}]}.


create('GET', [], {NavSpec, User}) ->
	{ok, Gateways} = k_lib:get_gtws(),
    {render_other, [{action, "edit_form"}], [
		{gateways, Gateways},
		{your_login, User:login()},
		NavSpec
		]};
create('POST', [], {_NavSpec, _User}) ->
	{ok, ID} = k_lib:create_provider(Req),
	{redirect, [{action, "update"}, {id, binary_to_list(ID)}]}.


update('GET', [ID], {NavSpec, User}) ->
	{ok, Provider}  = k_lib:get_provider(ID),
	{ok, Gateways} = k_lib:get_gtws(),
    {render_other, [{action, "edit_form"}], [
		{gateways, Gateways},
		{your_login, User:login()},
		NavSpec,
		{provider, Provider}
	   	]};
update('POST', [ID], {_NavSpec, _User}) ->
	{ok, _Gateway} = k_lib:update_provider(ID, Req),
	{redirect, [{action, "update"}, {id, ID}]}.
