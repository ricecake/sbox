Nonterminals
statement params kv_pair kv_pairs fields.

Terminals label string set sep readline.

Rootsymbol statement.

statement -> label params : {strip('$1'), '$2'}.

params    -> label  : strip('$1').
params    -> string : strip('$1').

params    -> label  fields   : {strip('$1'), '$2'}.
params    -> string fields   : {strip('$1'), '$2'}.

params    -> label kv_pairs : {strip('$1'), '$2'}.
params    -> string kv_pairs: {strip('$1'), '$2'}.

fields    -> label   : [strip('$1')].
fields    -> string  : [strip('$1')].

fields    -> label  sep fields : [strip('$1')| '$2'].
fields    -> string sep fields : [strip('$1')| '$2'].

kv_pairs  -> kv_pair : ['$1'].
kv_pairs  -> kv_pair sep kv_pairs : ['$1' | '$3'].

kv_pair   -> string set string   : {strip('$1'), strip('$3')}.
kv_pair   -> label  set string   : {strip('$1'), strip('$3')}.
kv_pair   -> string set readline : {strip('$1'), strip('$3')}.
kv_pair   -> label  set readline : {strip('$1'), strip('$3')}.

Erlang code.
strip({_, Val})-> Val.

