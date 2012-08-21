-module(kadmin_kadmin_controller, [Req, SessionID]).

-compile([export_all, {parse_transform, lager_transform}]).

before_(_) ->
	auth_lib:require_authentication(SessionID).

index('GET', []) ->
	ok.
