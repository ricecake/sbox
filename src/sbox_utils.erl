-module(sbox_utils).

-export([init/1, init_table/2]).

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

