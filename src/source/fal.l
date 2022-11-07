%%
[0-9_]+ "INT_LITERAL"
[0-9]+\.[0-9]* "FLOATING_LITERAL"
true "TRUE_LITERAL"
false "FALSE_LITERAL"
var "VAR"
let "LET"
fn "FN"
None "NONE_LITERAL"
Some "SOME"
if "IF"
else "ELSE"
while "WHILE"
[\w_][\w_0-9]* "ID"
"(?:\\"|[^"\n])*" "STRING_LITERAL"
\+ "PLUS"
- "MINUS"
\+\+ "CONCAT"
\* "MUL"
-> "RET"
\*\* "POW"
/ "DIV"
% "MOD"
= "EQ"
== "EQT"
!= "NEQ"
> "GT"
>= "GTE"
< "LT"
<= "LTE"
<< "SHL"
>> "SHR"
: "COLON"
; "SEMICOLON"
& "AMP"
| "PIPE"
|| "OR"
&& "AND"
\^ "HAT"
, "COMMA"
! "EXCL"
\? "QUEST"
@ "AT"
\. "DOT"
\( "LPAREN"
\) "RPAREN"
\[ "LBRACK"
\] "RBRACK"
\{ "LBRACE"
\} "RBRACE"
\z "EOF"
[ \t\n\r]+ ;
^//[^\n]*$ ;
. "UNMATCHED"