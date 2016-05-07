%{  /* Lex/Flex code for a scanner for the compiler project language */
enum { ENDOFINPUT = 0,
       SEMICOLON, COMMA, ENDOFPROGRAM, LEFTPARENTHESIS, RIGHTPARENTHESIS,
       ERROR, ASSIGNMENT, ADD, SUBTRACT, MULTIPLY, DIVIDE, EQUALITY,
       LESS, LESSEQUAL, GREATER, GREATEREQUAL, PROGRAM, VAR, PROCEDURE,
       REF, BEGINBLOCK, ENDBLOCK, WHILE, DO, IF, THEN, ELSE,
       READ, WRITE, INTCONST, IDENTIFIER, ILLEGALCHAR };
%}
alpha            [A-Za-z]
digit            [0-9]
alnum            {alpha}|{digit}
whitespace       [ \t\n]
%%
{whitespace}+    ;
"!".*            ;
";"              return SEMICOLON;
","              return COMMA;
"."              return ENDOFPROGRAM;
"("              return LEFTPARENTHESIS;
")"              return RIGHTPARENTHESIS;
":"              return ERROR;
":="             return ASSIGNMENT;
"+"              return ADD;
"-"              return SUBTRACT;
"*"              return MULTIPLY;
"/"              return DIVIDE;
"="              return EQUALITY;
"<"              return LESS;
"<="             return LESSEQUAL;
">"              return GREATER;
">="             return GREATEREQUAL;
PROGRAM          return PROGRAM;
VAR              return VAR;
PROCEDURE        return PROCEDURE;
REF              return REF;
BEGIN            return BEGINBLOCK;
END              return ENDBLOCK;
WHILE            return WHILE;
DO               return DO;
IF               return IF;
THEN             return THEN;
ELSE             return ELSE;
READ             return READ;
WRITE            return WRITE;
{digit}+         return INTCONST;
{alpha}{alnum}*  return IDENTIFIER;
.                return ILLEGALCHAR;

%%

int main()
{
    static char *tokennames[] = { "ENDOFINPUT",
        "SEMICOLON", "COMMA", "ENDOFPROGRAM", "LEFTPARENTHESIS", "RIGHTPARENTHESIS",
        "ERROR", "ASSIGNMENT", "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "EQUALITY",
        "LESS", "LESSEQUAL", "GREATER", "GREATEREQUAL", "PROGRAM", "VAR", "PROCEDURE",
        "REF", "BEGINBLOCK", "ENDBLOCK", "WHILE", "DO", "IF", "THEN", "ELSE",
        "READ", "WRITE", "INTCONST", "IDENTIFIER", "ILLEGALCHAR" };

    int token;

    do {
       token = yylex();
        if ( token < 0 || token > ILLEGALCHAR ) {
	    printf("Error, illegal token code %d returned\n", token);
	    return 1;
	}
        printf("Read %s", tokennames[token]);
	if ( token == IDENTIFIER || token == INTCONST ) printf(" \"%s\"", yytext);
	putchar('\n');
    }
    while (ENDOFINPUT != token);
    return 0;
}
