%{
/*
 *  dtof.l - Lex source file to convert all "double" keywords to
 *           "float"s, and to convert scanf and printf format
 *           statements appropriately.
 */
char *s;
%}
%%
double         printf("float");
\%[0-9.]*lf    { for (s = yytext; *s != 'l'; s++)
                   putchar(*s);
                 putchar('f');
               }
%%
int main(void)
{
  return yylex();
}
