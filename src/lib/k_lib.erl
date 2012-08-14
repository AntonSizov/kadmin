-module(k_lib).

-compile([{parse_transform, lager_transform}]).

-export([
	get_gtws/0,
	get_gtw/1,
	create_gtw/2,
	delete_gtw/1,
	update_gtw/3,

	create_connection/2,
	update_connection/3,
	del_connection/2,

	get_providers/0,
	get_provider/1,
	create_provider/1,
	update_provider/2,
	del_provider/1
]).

-record(k_props, {
	host :: string(),
	port :: string(),
	timeout :: string()
}).

update_provider(ID, Req) ->
	Gtw = Req:post_param("gateway"),
	BulkGtw = Req:post_param("bulk_gateway"),
	ReceiptsSupported = Req:post_param("receipts_supported"),
	ReqBody = "gateway=" ++ Gtw ++ "&bulk_gateway=" ++ BulkGtw ++ "&receipts_supported=" ++ ReceiptsSupported,
	{ok, KProps} = get_kelly_props(),
	Host = KProps#k_props.host,
	Port = KProps#k_props.port,
	Url = "http://" ++ Host ++ ":" ++ Port ++ "/providers/" ++ ID,
	lager:debug("Url: ~p, ReqBody: ~p", [Url, ReqBody]),
	{ok, {{_, 200, _}, _Headers, Body}} =
	httpc:request(put,	{Url, [], "application/x-www-form-urlencoded", ReqBody},[],[{body_format, binary}]),
	Provider = jsx:decode(Body),
	{ok, Provider}.


get_provider(ID) ->
	{ok, KProps} = get_kelly_props(),
	Host = KProps#k_props.host,
	Port = KProps#k_props.port,
	Timeout = KProps#k_props.timeout,
	Url = "http://" ++ Host ++ ":" ++ Port ++ "/providers/" ++ ID,
	lager:debug("Url: ~p", [Url]),
	{ok, {_StatusLine, _Headers, Body}} = httpc:request(get, {Url, []}, [{timeout, Timeout}, {connection_timeout, Timeout}], [{body_format, binary}]),
	Provider = jsx:decode(Body),
	{ok, Provider}.

get_providers() ->
	{ok, KProps} = get_kelly_props(),
	Host = KProps#k_props.host,
	Port = KProps#k_props.port,
	Timeout = KProps#k_props.timeout,
	Url = "http://" ++ Host ++ ":" ++ Port ++ "/providers",
	lager:debug("Url: ~p", [Url]),
	{ok, {_StatusLine, _Headers, Body}} = httpc:request(get, {Url, []}, [{timeout, Timeout}, {connection_timeout, Timeout}], [{body_format, binary}]),
	lager:debug("Body: ~p", [Body]),
	[{_, Providers}] = jsx:decode(Body),
	{ok, Providers}.

create_provider(Req) ->
	Gtw = Req:post_param("gateway"),
	BulkGtw = Req:post_param("bulk_gateway"),
	ReceiptsSupported = Req:post_param("receipts_supported"),
	ReqBody = "gateway=" ++ Gtw ++ "&bulk_gateway=" ++ BulkGtw ++ "&receipts_supported=" ++ ReceiptsSupported,
	{ok, KProps} = get_kelly_props(),
	Host = KProps#k_props.host,
	Port = KProps#k_props.port,
	Url = "http://" ++ Host ++ ":" ++ Port ++ "/providers",
	lager:debug("Url: ~p, ReqBody: ~p", [Url, ReqBody]),
	{ok, {_StatusLine, _Headers, Body}} =
	httpc:request(post,	{Url, [], "application/x-www-form-urlencoded", ReqBody},[],[{body_format, binary}]),
	Provider = jsx:decode(Body),
	ID = proplists:get_value(<<"id">>, Provider),
	{ok, ID}.

del_provider(ID) ->
	{ok, KProps} = get_kelly_props(),
	Host = KProps#k_props.host,
	Port = KProps#k_props.port,
	Url = "http://" ++ Host ++ ":" ++ Port ++ "/providers/" ++ ID,
	lager:debug("Url: ~p", [Url]),
	{ok, {{_Ver, 204, _Message}, _Headers, _Body}} =
	httpc:request(delete, {Url, []}, [], [{body_format, binary}]),
	ok.

update_connection(GtwID, ConnID, Req) ->
	Type = Req:post_param("type"),
	Addr = Req:post_param("addr"),
	Port = Req:post_param("port"),
	SysID = Req:post_param("sys_id"),
	Pass = Req:post_param("pass"),
	SysType = Req:post_param("sys_type"),
	AddrTON = Req:post_param("addr_ton"),
	AddrNPI = Req:post_param("addr_npi"),
	AddrRange = Req:post_param("addr_range"),
	ReqBody = "type=" ++ Type ++ "&addr=" ++ Addr ++ "&port=" ++ Port ++ "&sys_id=" ++ SysID ++ "&pass=" ++ Pass ++ "&sys_type=" ++ SysType ++ "&addr_ton=" ++ AddrTON ++ "&addr_npi=" ++ AddrNPI ++ "&addr_range=" ++ AddrRange,
	{ok, KProps} = get_kelly_props(),
	Host = KProps#k_props.host,
	KPort = KProps#k_props.port,
	Url = "http://" ++ Host ++ ":" ++ KPort ++ "/gateways/" ++ GtwID ++ "/connections/" ++ ConnID,
	lager:debug("Url: ~p, ReqBody: ~p", [Url, ReqBody]),
	{ok, {{_, 200, _}, _Headers, Body}} =
	httpc:request(put,	{Url, [], "application/x-www-form-urlencoded", ReqBody},[],[{body_format, binary}]),
	Connection = jsx:decode(Body),
	{ok, Connection}.

del_connection(GtwID, ConnID) ->
	{ok, KProps} = get_kelly_props(),
	Host = KProps#k_props.host,
	Port = KProps#k_props.port,
	Url = "http://" ++ Host ++ ":" ++ Port ++ "/gateways/" ++ GtwID ++ "/connections/" ++ ConnID,
	lager:debug("Url: ~p", [Url]),
	{ok, {{_Ver, 204, _Message}, _Headers, _Body}} =
	httpc:request(delete, {Url, []}, [], [{body_format, binary}]),
	ok.

create_connection(GtwID, Req) ->
	Type = Req:post_param("type"),
	Addr = Req:post_param("addr"),
	Port = Req:post_param("port"),
	SysID = Req:post_param("sys_id"),
	Pass = Req:post_param("pass"),
	SysType = Req:post_param("sys_type"),
	AddrTON = Req:post_param("addr_ton"),
	AddrNPI = Req:post_param("addr_npi"),
	AddrRange = Req:post_param("addr_range"),
	ReqBody = "type=" ++ Type ++ "&addr=" ++ Addr ++ "&port=" ++ Port ++ "&sys_id=" ++ SysID ++ "&pass=" ++ Pass ++ "&sys_type=" ++ SysType ++ "&addr_ton=" ++ AddrTON ++ "&addr_npi=" ++ AddrNPI ++ "&addr_range=" ++ AddrRange,
	{ok, KProps} = get_kelly_props(),
	Host = KProps#k_props.host,
	KPort = KProps#k_props.port,
	Url = "http://" ++ Host ++ ":" ++ KPort ++ "/gateways/" ++ GtwID ++ "/connections/",
	lager:debug("Url: ~p, ReqBody: ~p", [Url, ReqBody]),
	{ok, {{_, 201, _}, _Headers, Body}} =
	httpc:request(post,	{Url, [], "application/x-www-form-urlencoded", ReqBody},[],[{body_format, binary}]),
	Connection = jsx:decode(Body),
	{ok, Connection}.


update_gtw(ID, Name, RPS) ->
	ReqBody = "name=" ++ Name ++ "&" ++ "rps=" ++ RPS,
	{ok, KProps} = get_kelly_props(),
	Host = KProps#k_props.host,
	Port = KProps#k_props.port,
	Url = "http://" ++ Host ++ ":" ++ Port ++ "/gateways/" ++ ID,
	lager:debug("Url: ~p, ReqBody: ~p", [Url, ReqBody]),
	{ok, {{_, 200, _}, _Headers, Body}} =
	httpc:request(put,	{Url, [], "application/x-www-form-urlencoded", ReqBody},[],[{body_format, binary}]),
	Gateway = jsx:decode(Body),
	{ok, Gateway}.


delete_gtw(ID) ->
	{ok, KProps} = get_kelly_props(),
	Host = KProps#k_props.host,
	Port = KProps#k_props.port,
	Url = "http://" ++ Host ++ ":" ++ Port ++ "/gateways/" ++ ID,
	lager:debug("Url: ~p", [Url]),
	{ok, {{_Ver, 204, _Message}, _Headers, _Body}} =
	httpc:request(delete, {Url, []}, [], [{body_format, binary}]),
	ok.


create_gtw(Name, RPS) ->
	ReqBody = "name=" ++ Name ++ "&" ++ "rps=" ++ RPS,
	{ok, KProps} = get_kelly_props(),
	Host = KProps#k_props.host,
	Port = KProps#k_props.port,
	Url = "http://" ++ Host ++ ":" ++ Port ++ "/gateways",
	lager:debug("Url: ~p, ReqBody: ~p", [Url, ReqBody]),
	{ok, {_StatusLine, _Headers, Body}} =
	httpc:request(post,	{Url, [], "application/x-www-form-urlencoded", ReqBody},[],[{body_format, binary}]),
	Gateway = jsx:decode(Body),
	ID = proplists:get_value(<<"id">>, Gateway),
	{ok, ID}.


get_gtws() ->
	{ok, KProps} = get_kelly_props(),
	Host = KProps#k_props.host,
	Port = KProps#k_props.port,
	Timeout = KProps#k_props.timeout,
	Url = "http://" ++ Host ++ ":" ++ Port ++ "/gateways",
	lager:debug("Url: ~p", [Url]),
	{ok, {_StatusLine, _Headers, Body}} = httpc:request(get, {Url, []}, [{timeout, Timeout}, {connection_timeout, Timeout}], [{body_format, binary}]),
	[{_, Gateways}] = jsx:decode(Body),
	{ok, Gateways}.

get_gtw(ID) ->
	{ok, KProps} = get_kelly_props(),
	Host = KProps#k_props.host,
	Port = KProps#k_props.port,
	Timeout = KProps#k_props.timeout,
	Url = "http://" ++ Host ++ ":" ++ Port ++ "/gateways/" ++ ID,
	lager:debug("Url: ~p", [Url]),
	{ok, {_StatusLine, _Headers, Body}} = httpc:request(get, {Url, []}, [{timeout, Timeout}, {connection_timeout, Timeout}], [{body_format, binary}]),
	Result = jsx:decode(Body),
	{ok, Result}.


get_kelly_props() ->
	[HostEntry] = boss_db:find(kadmin_settings, [{key, 'equals', "kelly_host"}]),
	Host = HostEntry:value(),
	[PortEntry] = boss_db:find(kadmin_settings, [{key, 'equals', "kelly_port"}]),
	Port = PortEntry:value(),
	[TimeoutEntry] = boss_db:find(kadmin_settings, [{key, 'equals', "kelly_timeout"}]),
	Timeout = TimeoutEntry:value(),
	lager:debug("Host: ~p, Port: ~p, Timeout: ~p", [Host, Port, Timeout]),
	{ok, #k_props{
			host = Host,
			port = Port,
			timeout = Timeout}}.


