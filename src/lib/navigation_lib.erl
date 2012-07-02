-module(navigation_lib).

-export([get_navigation/0]).

get_navigation() ->
    NavSpec =
    {navigation, [
        [   {name, "System account administration"},
            {methods, [
                    [{name, "Users"},
                    {link, "/users/index"}]
                    ]}
        ]
    ]},
    {ok, NavSpec}.
