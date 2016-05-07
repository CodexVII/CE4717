%{
/*
 *  testclass.l - Test the behaviour of special characters inside
 *                a character class. Within the character class 
 *                [+*.], the special characters "+", "*" and "." 
 *                loose their special meanings.
 */
%}
%%
[ \t\n]+	printf("space \"%s\"\n", yytext);
[+*.]+		printf("character class [+*.] instance \"%s\"\n", yytext);
.		printf("other \"%s\"\n", yytext);
%%
int main(void)
{
  return yylex();
}
