-module(sbox_utils).

-export([init/1, init_table/2, pad/2, crypt/2, decrypt/2, raw_decrypt/2, mk_key/1, passwd/0, hash/1, hash/2]).

init_table(Table, Opts) ->
	ok = case mnesia:create_table(Table, [{disc_copies, [node()]} |Opts]) of
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

hash(Data) when is_list(Data) -> hash(list_to_binary(Data));
hash(Data) when is_binary(Data) -> crypto:hash(sha256, Data).

hash(Salt, Data) when is_list(Data) -> hash(Salt, list_to_binary(Data));
hash(Salt, Data) when is_binary(Data) -> hash(<< Salt/binary, Data/binary >>).

passwd() -> erlang:integer_to_binary(binary:decode_unsigned(crypto:rand_bytes(16)), 36).

mk_key(PassPhrase) when is_list(PassPhrase) -> mk_key(list_to_binary(PassPhrase));
mk_key(PassPhrase) when is_binary(PassPhrase) -> crypto:hash(sha256, PassPhrase).

pad(Width,Binary) when size(Binary) rem Width =:= 0 -> Binary;
pad(Width,Binary) -> <<Binary/binary, (crypto:rand_bytes(Width - (size(Binary) rem Width)))/bits >>.

crypt(Key, Data) when is_list(Key) -> crypt(list_to_binary(Key), Data);
crypt(Key, Data) ->
	Prepped = << 16#deadbeef:32, (size(Data)):32, Data/binary >>,
	Packed  = pad(16, Prepped),
	Iv      = crypto:rand_bytes(16),
	Cipher  = crypto:block_encrypt(aes_cbc256, Key, Iv, Packed),
	{ok, << Iv:128/bits, Cipher/binary >>}.

decrypt(Key, Data) when is_list(Key) -> decrypt(list_to_binary(Key), Data);
decrypt(Key, <<Iv:128/bits, Cipher/binary>>) ->
	<< 16#deadbeef:32, Size:32, PlainText:Size/binary, _Padding/bits >> = crypto:block_decrypt(aes_cbc256, Key, Iv, Cipher),
	{ok, PlainText}.

raw_decrypt(Key, Data) when is_list(Key) -> raw_decrypt(list_to_binary(Key), Data);
raw_decrypt(Key, Data) ->
	{ok, PlainText} = decrypt(Key, Data),
	PlainText.
