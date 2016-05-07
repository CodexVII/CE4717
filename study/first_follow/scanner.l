%{
#include "ff.h"
%}
%%
[A-Z]		|
\<[^\>\n]+\>	return  NONTERMINAL;
[a-z]		|
\"[^\"\n]+\"	return  TERMINAL;
"->"		|
"-->"		|
":=="		return  EQUIVALANCE;
"|"		return  ALTERNATION;
"["		return  LEFTSQB;
"]"		return  RIGHTSQB;
"("		return  LEFTPAR;
")"		return  RIGHTPAR;
"{"		return  LEFTCYB;
"}"		return  RIGHTCYB;
";"		return  SEMICOLON;
\\eps		|
\\epsilon	|
\\lambda	return  NULLSTRING;
[ \t\n]		;
"%"[^\n]*	;
.		return  BADTOKEN;
