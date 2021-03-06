-module(kadmin_providers_controller, [Req, SessionID]).

-compile([{parse_transform, lager_transform}, export_all]).

-include("settings.hrl").

before_(_) ->
    auth_lib:require_authentication(SessionID).

index('GET', []) ->
	{ok, Providers} = k_lib:get_providers(),
	Total = length(Providers),
    {ok, [
		{providers, Providers},
		{total_rows, Total},
		{active_page, 1},
		{page_numbers, [1]}
	]}.

delete('GET', [ID]) ->
	boss_flash:add(SessionID, notice, "Action result", io_lib:format("Provider ~p was successfully deleted", [ID])),
	ok = k_lib:del_provider(ID),
	{redirect, [{action, "index"}]}.


create('GET', []) ->
	{ok, Gateways} = k_lib:get_gtws(),
    {render_other, [{action, "edit_form"}], [
		{gateways, Gateways}
		]};
create('POST', []) ->
	{ok, ID} = k_lib:create_provider(Req),
	{redirect, [{action, "update"}, {id, binary_to_list(ID)}]}.


update('GET', [ID]) ->
	{ok, Provider}  = k_lib:get_provider(ID),
	{ok, Gateways} = k_lib:get_gtws(),
    {render_other, [{action, "edit_form"}], [
		{gateways, Gateways},
		{provider, Provider}
	   	]};
update('POST', [ID]) ->
	{ok, _Gateway} = k_lib:update_provider(ID, Req),
	{redirect, [{action, "update"}, {id, ID}]}.
