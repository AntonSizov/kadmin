-module(kadmin_gateways_controller, [Req, SessionID]).

-compile([{parse_transform, lager_transform}, export_all]).

-include("settings.hrl").

before_("login") -> ok;
before_(_) ->
    auth_lib:require_authentication(SessionID).

index('GET', [], {NavSpec, User}) ->
	{ok, Gateways} = k_lib:get_gtws(),
	lager:debug("Gateways: ~p", [Gateways]),
	Total = length(Gateways),
    {ok, [
		{gateways, Gateways},
		{your_login, User:login()},
		{total_rows, Total},
		{active_page, 1},
		{page_numbers, [1]},
		NavSpec]}.

delete('GET', [ID], {_NavSpec, _}) ->
	boss_flash:add(SessionID, notice, "Action result", io_lib:format("Gateway ~p was successfully deleted", [ID])),
	ok = k_lib:delete_gtw(ID),
	{redirect, [{action, "index"}]}.


create('GET', [], {NavSpec, User}) ->
    {render_other, [{action, "edit_form"}], [
		{your_login, User:login()},
		NavSpec
		]};
create('POST', [], {_NavSpec, _User}) ->
	Name = Req:post_param("name"),
	RPS = Req:post_param("rps"),
	{ok, ID} = k_lib:create_gtw(Name, RPS),
	{redirect, [{action, "update"}, {id, binary_to_list(ID)}]}.


update('GET', [ID], {NavSpec, User}) ->
	{ok, Gateway}  = k_lib:get_gtw(ID),
	lager:debug("Gateway: ~p", [Gateway]),
    {render_other, [{action, "edit_form"}], [
		{your_login, User:login()},
		NavSpec,
		{gateway, Gateway}
	   	]};
update('POST', [ID], {_NavSpec, _User}) ->
	Name = Req:post_param("name"),
	RPS = Req:post_param("rps"),
	{ok, _Gateway} = k_lib:update_gtw(ID, Name, RPS),
	{redirect, [{action, "update"}, {id, ID}]}.


delete_connection('GET', [GtwID, ConID], _) ->
	ok = k_lib:del_connection(GtwID, ConID),
	{redirect, [{action, "update"}, {id, GtwID}]}.

update_connection('POST', [GtwID, ConnID], _) ->
	{ok, _Connection} = k_lib:update_connection(GtwID, ConnID, Req),
	{redirect, [{action, "update"}, {id, GtwID}]}.

create_connection('POST', [GtwID], _) ->
	{ok, _Connection} = k_lib:create_connection(GtwID, Req),
	{redirect, [{action, "update"}, {id, GtwID}]}.


