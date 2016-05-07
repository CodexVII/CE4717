#define  ENDOFFILE        0	/* 0 is returned by yylex on end of input */
#define  TERMINAL	257
#define  NONTERMINAL	258
#define  EQUIVALANCE    259
#define  ALTERNATION    260
#define  LEFTSQB   	261
#define  RIGHTSQB	262
#define  LEFTPAR        263
#define  RIGHTPAR       264
#define  LEFTCYB        265
#define  RIGHTCYB	270
#define  SEMICOLON      271
#define  NULLSTRING     272	/* lambda or epsilon */
#define  ENDINPUTSYM    273	/* an explicit end of production '$' */
#define  BADTOKEN	274

#define  MAXPRODUCTIONS        100  /* maximum no. of productions	*/
#define  MAXSYMBOLSPERPROD      10  /* max no. of symbols per prod.	*/

#define  INTERNALSYMBOL       -100  /* Goes in the "class" field of an  */
				    /* internally generated symbol      */
#ifdef   PARSER
extern int  yylex(void);
extern int  yyleng;
/* flex yytext */
extern char *yytext;
/* lex yytext */
/* extern char yytext[]; */
#endif   



