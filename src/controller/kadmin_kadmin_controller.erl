-module(kadmin_kadmin_controller, [Req, SessionID]).

-compile([export_all, {parse_transform, lager_transform}]).

before_(_) ->
	auth_lib:require_authentication(SessionID).

index('GET', _, {NavSpec, User}) ->
	lager:debug("User: ~p", [User]),
	{ok, [{your_login, User:login()}, NavSpec]}.

% hello('GET', _) ->
% 	{ok, [{greeting, "Hello, world!"}]}.
% 	% {output, "Hello, world!"}.
% 	% {json, [{greeting, "Hello, world!"}]}.

% list('GET', []) ->
% 	Greetings = boss_db:find(greeting, []),
% 	{ok, [{greetings, Greetings}]}.

% create('GET', []) ->
% 	ok;
% create('POST', []) ->
% 	GreetingText = Req:post_param("greeting_text"),
% 	NewGreeting = greeting:new(id, GreetingText),
% 	case NewGreeting:save() of
% 		{ok, SavedGreeting} ->
% 			{redirect, [{action, "list"}]};
% 		{error, ErrorList} ->
% 			{ok, [{errors, ErrorList}, {new_msg, NewGreeting}]}
% 	end.

% goodbye('POST', []) ->
% 	boss_db:delete(Req:post_param("greeting_id")),
% 	{redirect, [{action, "list"}]}.

% send_test_message('GET', []) ->
% 	TestMessage = "Free at last!",
% 	boss_mq:push("test-channel", TestMessage),
% 	{output, TestMessage}.

% pull('GET', [LastTimestamp]) ->
% 	io:format("Pull. LastTimestamp: ~p~n", [LastTimestamp]),
% 	{ok, Timestamp, Greetings} = boss_mq:pull("new-greetings",
% 												list_to_integer(LastTimestamp)),
% 	io:format("New Timestamp: ~p~n", [Timestamp]),
% 	io:format("New Greetings: ~p~ns", [Greetings]),
% 	{json, [{timestamp, Timestamp}, {greetings, Greetings}]}.

% live('GET', []) ->
% 	Greetings = boss_db:find(greeting, []),
% 	Timestamp = boss_mq:now("new-greetings"),
% 	{ok, [{greetings, Greetings}, {timestamp, Timestamp}]}.
