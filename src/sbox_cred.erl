-module(sbox_cred).

-export([init/0, create/3, list/1, get/2]).

-record(cred, {
	id,
	owner,
	system,
	details
}).

init() -> sbox_utils:init_table(cred,   [{attributes, record_info(fields, cred)}]).

create({UserName, Salt, RawKey}, System, Details) ->
	Id     = sbox_utils:hash(<< Salt/binary, System/binary >>),
	SecOwn = sbox_utils:hash(<< Salt/binary, UserName/binary >>),
	{ok, SecSys} = sbox_utils:crypt(RawKey, System),
	{ok, SecDet} = sbox_utils:crypt(RawKey, Details),
	{atomic, Result}  = mnesia:transaction(fun()->
		mnesia:write(#cred{
			id     =Id,
			owner  =SecOwn,
			system =SecSys,
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


get({_UserName, Salt, RawKey}, System) ->
	Id     = sbox_utils:hash(<< Salt/binary, System/binary >>),
	{atomic, Cred} = mnesia:transaction(fun()->
		case mnesia:read({cred, Id}) of
			[]    -> undefined;
			[Row] -> sbox_utils:decrypt(RawKey, Row#cred.details)
		end
	end),
	Cred.
