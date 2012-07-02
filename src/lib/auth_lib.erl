-module(auth_lib).

-compile({parse_transform, lager_transform}).

-export([require_authentication/1]).

require_authentication(SessionId) ->
    Session = boss_session:get_session_data(SessionId, principal),
    case Session of
        undefined ->
            {redirect, "/login"};
        {error, Reason} ->
			lager:error("~p", [Reason]),
            {redirect, "/login"};
        UserId ->
            User = boss_db:find(UserId),
            case User of
                {error, Reason} ->
                    boss_flash:add(SessionId, error, "User not found", Reason),
                    {redirect, "/login"};
                _ ->
                    {ok, NavSpec} = navigation_lib:get_navigation(),
                    {ok, {NavSpec, User}}
            end
    end.
