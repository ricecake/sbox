-module(sbox).

-export([start/0, init/0]).

start() -> application:ensure_all_started(sbox).

init() ->
	sbox_utils:init([]),
	sbox_user:init(),
	sbox_cred:init(),
	{ok, sbox}.
