-module(sbox_cred).

-export([init/0, create/3, list/1, get/2]).

-record(cred, {
	system,
	owner,
	details
}).

init() -> sbox_utils:init_table(cred,   [{attributes, record_info(fields, cred)}]).

create({UserName, Salt, RawKey}, System, Details) ->
	SecOwn = sbox_utils:hash(<< Salt/binary, UserName/binary >>),
	{ok, SecSys} = sbox_utils:crypt(RawKey, System),
	{ok, SecDet} = sbox_utils:crypt(RawKey, Details),
	{atomic, Result}  = mnesia:transaction(fun()->
		mnesia:write(#cred{
			system =SecSys,
			owner  =SecOwn,
			details=SecDet
		})
	end),
	Result.

list({UserName, Salt, RawKey}) ->
	SecOwn = sbox_utils:hash(<< Salt/binary, UserName/binary >>),
	{atomic, Creds} = mnesia:transaction(fun()->
		[sbox_utils:raw_decrypt(RawKey, Row#cred.system) || Row <- mnesia:match_object(#cred{owner=SecOwn, _='_'})]
	end),
	{ok, Creds}.


get({UserName, Salt, RawKey} = Cred, System) ->
	CredList = list(Cred),
	case proplists:get_value(System, CredList) of
		undefined -> undefined;
		Details   -> sbox_utils:decrypt(RawKey, Details)
	end.
