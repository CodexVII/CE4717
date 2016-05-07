%{
#define R1 '1'
#define R2 '2'
#define R3 '3'
#define R4 '4'
#define R5 '5'
#define R6 '6'
%}

%%
[ \t\n]                                     ;
[0-9a-fA-F]+[hH]                           R1;
-?[0-9]+                                   R2;
[A-Za-z][A-Za-z0-9]*                       R3;
-?[0-9]+(\.[0-9]+)?([eE][+|-]?[0-9]+)?     R4;
[01][01]*[bB]                              R5;
.                                          R6;

%%
int main(void)
{
  int tcode;
  do{
    tcode = yylex();
    printf("token type %d \"%s\"\n", tcode, yytext);
  }while( tcode );
  return 0;
}
