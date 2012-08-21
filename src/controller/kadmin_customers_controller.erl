-module(kadmin_customers_controller, [Req, SessionID]).

-compile([{parse_transform, lager_transform}, export_all]).

-include("settings.hrl").

before_(_) ->
    auth_lib:require_authentication(SessionID).

test('GET', [], _) ->
	ok.

index('GET', []) ->
	{ok, Customers} = k_lib:get_customers(),
	lager:debug("Customers: ~p", [Customers]),
	Total = length(Customers),
    {ok, [
		{customers, Customers},
		{total_rows, Total},
		{active_page, 1},
		{page_numbers, [1]}
	]}.

delete('GET', [CustomerID]) ->
	ok = k_lib:del_customer(CustomerID),
	boss_flash:add(SessionID, notice, "Action result", io_lib:format("Customer ~p was successfully deleted", [CustomerID])),
	{redirect, [{action, "index"}]}.


create('GET', []) ->
	{ok, Providers} = k_lib:get_providers(),
	{ok, Networks} = k_lib:get_networks(),
    {render_other, [{action, "edit_form"}], [
		{providers, Providers},
		{networks, Networks}
		]};
create('POST', []) ->
	{ok, Customer} = k_lib:create_customer(Req),
	ID = proplists:get_value(<<"id">>, Customer),
	{redirect, [{action, "update"}, {id, binary_to_list(ID)}]}.


update('GET', [CustomerID]) ->
	{ok, Customer}  = k_lib:get_customer(CustomerID),
	lager:debug("Customer: ~p", [Customer]),
	{ok, Providers} = k_lib:get_providers(),
	{ok, Networks} = k_lib:get_networks(),
    {render_other, [{action, "edit_form"}], [
		{customer, Customer},
		{providers, Providers},
		{networks, Networks}
	   	]};
update('POST', [CustomerID]) ->
	{ok, _Customer} = k_lib:update_customer(CustomerID, Req),
	{redirect, [{action, "update"}, {customer_id, CustomerID}]}.


delete_user('GET', [CustomerID, UserID]) ->
	ok = k_lib:del_user(CustomerID, UserID),
	{redirect, [{action, "update"}, {customer_id, CustomerID}]}.

update_user('GET', [CustomerID, UserID]) ->
	{ok, CustomerUser} = k_lib:get_user(CustomerID, UserID),
	lager:debug("Customer user: ~p", [CustomerUser]),
	{render_other, [{action, "edit_user"}], [
		{customer_id, CustomerID},
		{user, CustomerUser},
		{'_smpp_types', smpp_types()}
	   	]};
update_user('POST', [CustomerID, UserID]) ->
	{ok, _Connection} = k_lib:update_user(CustomerID, UserID, Req),
	{redirect, [{action, "update_user"}, {customer_id, CustomerID}, {user_id, UserID}]}.

create_user('GET', [CustomerID]) ->
	{render_other, [{action, "edit_user"}], [
		{customer_id, CustomerID},
		{'_smpp_types', smpp_types()}
	   	]};
create_user('POST', [CustomerID]) ->
	{ok, _User} = k_lib:create_user(CustomerID, Req),
	{redirect, [{action, "update"}, {id, CustomerID}]}.


smpp_types() ->
	[<<"transceiver">>, <<"receiver">>, <<"transmitter">>].
