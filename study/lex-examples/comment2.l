%{
#include <stdio.h>
%}
%%
[ \t\n]+                             ;
"/*"			{
				int ch, state;
				printf("C comment /*");
				state = 0;
				while ( state != 2 && (ch = input()) != EOF ) {
				    putchar(ch);
				    switch ( state ) {
				        case 0:  if ( ch == '*' )  state = 1; break;
					case 1:  if ( ch == '/' )  state = 2; else if ( ch != '*' ) state = 0; break;
					default: break;
				    }
				}
				putchar('\n');
			}
[A-Za-z][A-Za-z0-9]*                 printf("identifier %s\n", yytext);
[0-9]+                               printf("intconst   %s\n", yytext);
.                                    printf("illegal    %s\n", yytext);
%%
int main(void)
{
    return yylex();
}
