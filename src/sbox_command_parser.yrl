Nonterminals
statement params kv_pair kv_pairs.

Terminals label string set sep readline.

Rootsymbol statement.

statement -> label params : {'$1', '$2'}.

params    -> label kv_pairs : {'$1', ['$2']}.
params    -> string kv_pairs: {'$1', ['$2']}.
kv_pairs  -> kv_pair : '$1'.
kv_pairs  -> kv_pair sep kv_pairs : ['$1' | '$2'].
kv_pair   -> string set string   : {'$1', '$2'}.
kv_pair   -> label  set string   : {'$1', '$2'}.
kv_pair   -> string set readline : {'$1', '$2'}.
kv_pair   -> label  set readline : {'$1', '$2'}.

Erlang code.

