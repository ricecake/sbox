-module(sbox_utils).

-export([init/1, init_table/2, pad/2, crypt/2, decrypt/2, mk_key/1]).

init_table(Table, Opts) ->
	ok = case mnesia:create_table(Table, Opts) of
		{atomic, ok} -> ok;
		{aborted, {already_exists, Table}} -> ok;
		{aborted, Reason} -> Reason
	end,
	{ok, Table}.	

init([]) ->
	Node = node(),
	ok = case mnesia:create_schema([Node]) of
		ok -> ok;
		{error, {Node, {already_exists, Node}}} -> ok;
		_ -> error
	end,
	mnesia:start().

mk_key(PassPhrase) when is_binary(PassPhrase) -> crypto:hash(sha256, PassPhrase).

pad(Width,Binary) when size(Binary) rem Width =:= 0 -> Binary;
pad(Width,Binary) -> <<Binary/binary, (crypto:rand_bytes(Width - (size(Binary) rem Width)))/bits >>.


crypt(Key, Data) ->
	Prepped = << 16#deadbeef:32, (size(Data)):32, Data/binary >>,
	Packed  = pad(16, Prepped),
	Iv      = crypto:rand_bytes(16),
	Cipher  = (crypto:block_encrypt(aes_cbc256, Key, Iv, Packed)),
	{ok, << Iv:128/bits, Cipher/binary >>}.

decrypt(Key, <<Iv:128/bits, Cipher/binary>>) ->
	<< 16#deadbeef:32, Size:32, PlainText:Size/binary, _Padding/bits >> = crypto:block_decrypt(aes_cbc256, Key, Iv, Cipher),
	{ok, PlainText}.
