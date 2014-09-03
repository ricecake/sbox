Definitions.

L   = [A-Za-z]
S   = [^\s]
WS  = ([\000-\s]|%.*)
C   = (=|:|=>)
SEP = (,|;)

Rules.

{Q}      : {token, {quote, atom(TokenChars)}}.
{C}      : {token, {set,   atom(TokenChars)}}.
{L}+     : {token, {label, input(TokenChars)}}.
{SEP}    : {token, {sep,   input(TokenChars)}}.
<>       : {token, {readline}}.
"{S}+"   : {token, {string,input(strip(1, TokenChars, TokenLen))}}.
'{S}+'   : {token, {string,input(strip(1, TokenChars, TokenLen))}}.
<<{S}+>> : {token, {string,input(strip(2, TokenChars, TokenLen))}}.
{WS}+    : skip_token.

Erlang code.

atom(TokenChars) -> list_to_atom(TokenChars).
strip(I, S, L) -> string:sub_string(S, I+1, L-I).
input(T) -> list_to_binary(T).
