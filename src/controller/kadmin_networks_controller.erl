-module(kadmin_networks_controller, [Req, SessionID]).

-compile([{parse_transform, lager_transform}, export_all]).

-include("settings.hrl").

before_(_) ->
    auth_lib:require_authentication(SessionID).

index('GET', []) ->
	{ok, Networks} = k_lib:get_networks(),
	Total = length(Networks),
    {ok, [
		{networks, Networks},
		{total_rows, Total},
		{active_page, 1},
		{page_numbers, [1]}
	]}.

delete('GET', [ID]) ->
	boss_flash:add(SessionID, notice, "Action result", io_lib:format("Provider ~p was successfully deleted", [ID])),
	ok = k_lib:del_network(ID),
	{redirect, [{action, "index"}]}.


create('GET', []) ->
	{ok, Providers}  = k_lib:get_providers(),
    {render_other, [{action, "edit_form"}], [
		{providers, Providers}
		]};
create('POST', []) ->
	{ok, Network} = k_lib:create_network(Req),
	ID = proplists:get_value(<<"id">>, Network),
	{redirect, [{action, "update"}, {id, binary_to_list(ID)}]}.


update('GET', [ID]) ->
	{ok, Providers}  = k_lib:get_providers(),
	{ok, Network} = k_lib:get_network(ID),
    {render_other, [{action, "edit_form"}], [
		{network, Network},
		{providers, Providers}
	   	]};
update('POST', [ID]) ->
	{ok, _Gateway} = k_lib:update_network(ID, Req),
	{redirect, [{action, "update"}, {id, ID}]}.
