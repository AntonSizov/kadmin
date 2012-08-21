-module(navigation_lib).

-export([get_navigation/0]).

get_navigation() ->
    NavSpec =
    {navigation, [
        [   {name, "Admin interface administration"},
            {methods, [
                    [{name, "Administrators"},
                    {link, "/users/index"}],

                    [{name, "Settings"},
                    {link, "/settings/index"}]

                    ]}
        ],
        [   {name, "Backend administration"},
            {methods, [

                    [{name, "Gateways"},
                    {link, "/gateways/index"}],

                    [{name, "Providers"},
                    {link, "/providers/index"}],

                    [{name, "Networks"},
                    {link, "/networks/index"}],

                    [{name, "Customers"},
                    {link, "/customers/index"}]

                    ]
			}
        ]
    ]},
    {ok, NavSpec}.
