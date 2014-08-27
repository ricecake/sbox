-module(sbox_user).

-export([init/0, create/2]).

-record(user, {
	user,
	pass,
	salt,
	syskey
}).

init() -> sbox_utils:init_table(user,   [{attributes, record_info(fields, user)}]).

create(UserName, Password) ->
	Salt = crypto:rand_bytes(16),
	{ok, SysKey} = sbox_utils:crypt(sbox_utils:mk_key(Password), crypto:rand_bytes(16)),
	UserData = #user{
		user=sbox_utils:hash(UserName),
		pass=sbox_utils:hash(<< Salt/binary, Password/binary >>),
		salt=Salt,
		syskey=SysKey
	},
	{atomic, Result} = mnesia:transaction(fun()->
		case mnesia:read({user, sbox_utils:hash(UserName)}) of
			[] -> mnesia:write(UserData);
			_  -> error
		end
	end),
	{Result, UserName}.
