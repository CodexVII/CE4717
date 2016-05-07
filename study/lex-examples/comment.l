%%
[ \t\n]+                             ;
"/*"[^*]*"*"(([^/*][^*]*)?"*")*"/"   printf("C comment  %s\n", yytext);
[A-Za-z][A-Za-z0-9]*                 printf("identifier %s\n", yytext);
[0-9]+                               printf("intconst   %s\n", yytext);
.                                    printf("illegal    %s\n", yytext);
%%
int main(void)
{
    return yylex();
}
