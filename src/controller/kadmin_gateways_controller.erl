-module(kadmin_gateways_controller, [Req, SessionID]).

-compile([{parse_transform, lager_transform}, export_all]).

-include("settings.hrl").

before_(_) ->
    auth_lib:require_authentication(SessionID).

index('GET', []) ->
	{ok, Gateways} = k_lib:get_gtws(),
	lager:debug("Gateways: ~p", [Gateways]),
	Total = length(Gateways),
    {ok, [
		{gateways, Gateways},
		{total_rows, Total},
		{active_page, 1},
		{page_numbers, [1]}
	]}.

delete('GET', [ID]) ->
	boss_flash:add(SessionID, notice, "Action result", io_lib:format("Gateway ~p was successfully deleted", [ID])),
	ok = k_lib:delete_gtw(ID),
	{redirect, [{action, "index"}]}.


create('GET', []) ->
    {render_other, [{action, "edit_form"}], []};
create('POST', []) ->
	Name = Req:post_param("name"),
	RPS = Req:post_param("rps"),
	{ok, ID} = k_lib:create_gtw(Name, RPS),
	{redirect, [{action, "update"}, {id, binary_to_list(ID)}]}.


update('GET', [ID]) ->
	{ok, Gateway}  = k_lib:get_gtw(ID),
	lager:debug("Gateway: ~p", [Gateway]),
    {render_other, [{action, "edit_form"}], [
		{gateway, Gateway}
	   	]};
update('POST', [ID]) ->
	Name = Req:post_param("name"),
	RPS = Req:post_param("rps"),
	{ok, _Gateway} = k_lib:update_gtw(ID, Name, RPS),
	{redirect, [{action, "update"}, {id, ID}]}.


delete_connection('GET', [GtwID, ConID]) ->
	ok = k_lib:del_connection(GtwID, ConID),
	{redirect, [{action, "update"}, {id, GtwID}]}.

update_connection('POST', [GtwID, ConnID]) ->
	{ok, _Connection} = k_lib:update_connection(GtwID, ConnID, Req),
	{redirect, [{action, "update"}, {id, GtwID}]}.

create_connection('POST', [GtwID]) ->
	{ok, _Connection} = k_lib:create_connection(GtwID, Req),
	{redirect, [{action, "update"}, {id, GtwID}]}.


