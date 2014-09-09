-module(sbox_user).

-export([init/0, create/2, auth/2]).

-record(user, {
	user,
	pass,
	salt,
	secsalt,
	syskey
}).

init() -> sbox_utils:init_table(user,   [{attributes, record_info(fields, user)}]).

create(UserName, Password) ->
	Salt = crypto:rand_bytes(16),
	{ok, SysKey} = sbox_utils:crypt(sbox_utils:mk_key(Password), crypto:rand_bytes(16)),
	{ok, SecSalt} = sbox_utils:crypt(sbox_utils:mk_key(Password), crypto:rand_bytes(16)),
	UserData = #user{
		user=sbox_utils:hash(UserName),
		pass=sbox_utils:hash(Salt, Password),
		salt=Salt,
		secsalt=SecSalt,
		syskey=SysKey
	},
	{atomic, Result} = mnesia:transaction(fun()->
		case mnesia:read({user, sbox_utils:hash(UserName)}) of
			[] -> mnesia:write(UserData);
			_  -> error
		end
	end),
	{Result, prepData(UserName, Password, UserData)}.

auth(UserName, Password) ->
	{atomic, Result} = mnesia:transaction(fun()->
		case mnesia:read({user, sbox_utils:hash(UserName)}) of
			[]     -> noauth;
			[User = #user{salt=Salt, pass=Pass}] ->
				case sbox_utils:hash(Salt, Password) of
					Pass -> {auth, prepData(UserName, Password, User)};
					_    -> noauth
				end
		end
	end),
	Result.

prepData(UserName, Password, #user{syskey=SysKey, secsalt=Salt}) ->
	{ok, RawKey}  = sbox_utils:decrypt(sbox_utils:mk_key(Password), SysKey),
	{ok, RawSalt} = sbox_utils:decrypt(sbox_utils:mk_key(Password), Salt),
	{UserName, RawSalt, RawKey}.
