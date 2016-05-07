%{
/*
 *  dtof2.l - Lex source file to convert all "double" keywords to
 *            "float"s, and to convert scanf and printf format
 *            statements appropriately.
 *
 *            Note that this source file provides definitions for
 *            both "main" and "yywrap", so does not require the
 *            library "flex.lib".
 *
 *            c:\> flex dtof2.l
 *            c:\> bcc32 -edtof2.exe lexyy.c
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

int yywrap(void)
{
  return 1;
}
