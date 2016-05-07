#include <stdio.h>
#include <stdlib.h>
#define  PARSER         1
#include "global.h"
#include "ff.h"
#include "symbol.h"
#include "strtab.h"

/*----------------------------------------------------------------------*/

#define  OPTION                  1  /* Used to tell "ProcessEBNF" the   */
#define  GROUPING                2  /* type of EBNF construction it is  */
#define  REPETITION              3  /* required to handle               */

#define  INTERNALNAME    "<I%04d>"  /* Form for internally generated    */
				    /* symbols                          */

#define  FIRSTPRODUCTION        1   /* indicates parse of 1st prod.     */

/*----------------------------------------------------------------------*/
/*                                                                      */
/*              Function prototypes for internal routines               */
/*                                                                      */
/*----------------------------------------------------------------------*/

void    ParseGrammar(void);
void    ParseProduction(int ProductionFlag);
void    ParseRightHandSide(SYMBOL *lhs);
void    ParseProductionList(void);
void    ParseSymbol(void);
void    ProcessEBNF(int ebnf_form);
void    Accept(int ExpectedToken);
void    NextToken(void);
void    SyntaxError(int ExpectedToken, int ActualToken);
char   *lookup(int token);
SYMBOL *InsertSymbol(int symboltype,char *symbolstring);
void    AddNewProduction(SYMBOL *ProductionName);
void    ShowProductions(void);
void    AddSymboltoProduction(SYMBOL *Symbol);
SYMBOL *GenerateInternalSymbol(void);
void    Dump(void);
void    Optimize(void);
void    RemoveGaps(void);

/*----------------------------------------------------------------------*/
/*                                                                      */
/*              Global variables                                        */
/*                                                                      */
/*----------------------------------------------------------------------*/

int  CurrentToken = ENDOFFILE;          /* current token from scanner   */

SYMBOL *NullString,                     /* used to hold symbol table    */
       *EndInputSym;                    /* pointers for "\epsilon" and  */
					/* "$"                          */
/*
	This 2-d array of pointers to symbols holds the productions.

	productions[i][0] = left hand side of production
	productions[i][j], j != 0 = right hand side of production,
	i.e., a list of the terminals and nonterminals in the
	production ( and \epsilon and $ ).

	The array hold the grammar in BNF -- the EBNF of the
	input is converted to BNF by the input parsing routines.

	As this is a static array, its initial contents are zeros.
	This is useful as it can be read as NULL pointers. Unused
	space for productions is detected by searching for a
	productions[i][0] entry  == NULL. The end the list of
	terminals, etc., forming the right hand side may similarly
	be detected by searching along a row for a NULL. Don't
	forget to check for boundary violations before doing this,
	however!
*/
SYMBOL *productions[MAXPRODUCTIONS][MAXSYMBOLSPERPROD];

/*
	An array of indices to the current insertion position on
	each row.
*/
int    CurrentSymbol[MAXPRODUCTIONS];

/*
	The current row index of the 2-d array is maintained in
	CurrentProduction.
*/
int    CurrentProduction;

/*----------------------------------------------------------------------*/

void ParseInput( void )
{
    /* make symbol table entries for the two special symbols which can
       occur in a BNF grammar, "\epsilon", i.e., the null string, and
       "$", the "end of input" symbol.
    */
    NullString  = InsertSymbol(NULLSTRING,"\\eps");
    EndInputSym = InsertSymbol(ENDINPUTSYM,"$");
    
    NextToken();        /* get the first token into CurrentToken */
    ParseGrammar();
    Optimize();
    RemoveGaps();	/* remove any empty entries		 */
    ShowProductions();
/*
    Dump();
*/
}

/*
	Parse the production

	<Grammar> :== <Production> ";" { <Production> ";" } $

*/
void ParseGrammar(void)
{
    ParseProduction(FIRSTPRODUCTION);
    Accept(SEMICOLON);
    while ( CurrentToken == NONTERMINAL)  {
	ParseProduction(!FIRSTPRODUCTION);
	Accept(SEMICOLON);
    }
    if ( CurrentToken != ENDOFFILE )  {
	fprintf(stderr,"Expected one of %s %s\n", lookup(NONTERMINAL), 
		       lookup(ENDOFFILE) );
	exit(EXIT_FAILURE);
    }
}

/*
	Parse the production

	<Production> :== <NonTerminal> "-->" <RightHandSide>

	If this is the first production (indicated by the "ProductionFlag"
	variable being equal to FIRSTPRODUCTION) a "wrapper" production
	is inserted in the internal production table which serves to
	wrap the entire grammar with an "end of input ($)" terminal
	symbol, i.e., say the external input is:

		A --> ....
		.
		.

	then the internal representation will be:

		<InternalSymbol> --> A $
		A --> ....
		.
		.

	this ensures that the FOLLOW set of A will always include
	the "$" symbol, and removes the possibility of null strings
	occurring in FOLLOW sets.

*/
void ParseProduction(int ProductionFlag)
{
    SYMBOL *lhs, *InternalSymbol;

    if ( CurrentToken == NONTERMINAL ) lhs = InsertSymbol(NONTERMINAL,yytext);
    Accept(NONTERMINAL);
    if ( ProductionFlag == FIRSTPRODUCTION )  {         /* create the   */
	InternalSymbol = GenerateInternalSymbol();      /* wrapper      */
	AddNewProduction(InternalSymbol);               /* production   */
	AddSymboltoProduction(lhs);
	AddSymboltoProduction(EndInputSym);
    }
    Accept(EQUIVALANCE);
    ParseRightHandSide(lhs);
}

/*
	Parse the production

	<RighthandSide> :== <ProductionList> { "|" <ProductionList> }

	A new production with the same left hand side is created for
	each option separated by the alternation operator. If we
	are dealing with productions with the same lhs as the
	start symbol a "$" symbol is added to the end of the internal
	representaions of each such production.
*/
void ParseRightHandSide(SYMBOL *lhs)
{
    AddNewProduction(lhs);
    ParseProductionList();
    while ( CurrentToken == ALTERNATION )  {
	AddNewProduction(lhs);
	NextToken();            /* remove the ALTERNATION symbol */
	ParseProductionList();
    }
}

/*
	Parse the production

	<ProductionList> :== NULLSTRING
	<ProductionList> :== <Symbol> { <Symbol> }

	Note -- we only allow null string symbols in very restricted 
	circumstances, by themselves on the right hand side of a production. 
	This is because their appearance in any other circumstance is totally
	redundant, and any grammar with null string symbols appearing 
	between other grammar symbols on its right hand side may be rewritten
	in an entirely equivalant form without the null string symbols.
	
	Restricting the appearance of null string symbols in this way helps
	to simplify the job of the grammar analysis algorithms.
*/
void ParseProductionList(void)
{
    ParseSymbol();
    while ( CurrentToken == NONTERMINAL || CurrentToken == TERMINAL ||
	    CurrentToken == LEFTSQB || CurrentToken == LEFTPAR ||
	    CurrentToken == LEFTCYB )
	ParseSymbol();
}

/*
	Parse the production

	<Symbol> :== <NonTerminal> "|" <Terminal> "|"
		     "[" <RightHandSide> "]" "|"
		     "(" <RightHandSide> ")" "|"
		     "{" <RightHandSide> "}" "|"
		     "epsilon"

	The three EBNF forms in this production are all converted
	to BNF using the same approach. A new, internal nonterminal
	is created to represent the entire <RightHandSide> contained
	between the "[ ]", "( )", or "{ }" and a new set of productions,
	the lhs of each set to this new nonterminal, created to handle 
	the <RightHandSide>. Of course, the right hand side may contain
	further nested EBNF operators, but these are handled recursively
	in the same way.

	Examples:

	EBNF     A  --> .... C [ D .... E ] F ....
	BNF      A  --> .... C T1 F ....
		 T1 --> \epsilon
		 T1 --> D .... E

	EBNF     A  --> .... C { D .... E } F .....
	BNF      A  --> .... C T1 F ....
		 T1 --> \epsilon
		 T1 --> T2 T1
		 T2 --> D .... E

	EBNF     A  --> .... C ( D1 | D2 | .... DN ) F .....
	BNF      A  --> .... C T1 F .....
		 T1 --> D1
		 T1 --> D2
		    .
		    .
		 T1 --> DN

	In case 2 a two stage rewriting is done. This is needed to
	cover the situation where the symbols between the "{ }"
	operators expand out into multiple productions. Rather than
	keep track of all of these and tag a T1 on to the end of
	each, this is handled by having one right-recursive production
	and making T2 a further subproduction of this. This may lead
	to BNF which is more long-winded than necessary, but never to
	incorrect BNF.

*/
void ParseSymbol(void)
{
    SYMBOL *s;

    switch ( CurrentToken )  {
	case  NONTERMINAL :  
	case  TERMINAL    : 
	    s = InsertSymbol(CurrentToken,yytext);
	    AddSymboltoProduction(s);
	    Accept(CurrentToken);     
	    break;
	case  LEFTSQB     :  Accept( LEFTSQB );
			     ProcessEBNF( OPTION );
			     Accept( RIGHTSQB );     break;
	case  LEFTPAR     :  Accept( LEFTPAR );
			     ProcessEBNF( GROUPING );
			     Accept( RIGHTPAR );     break;
	case  LEFTCYB     :  Accept( LEFTCYB );
			     ProcessEBNF( REPETITION );
			     Accept( RIGHTCYB );     break;
	case  NULLSTRING  :  
	    AddSymboltoProduction(NullString);
	    Accept( NULLSTRING );   
	    break;
	default: 
	    fprintf(stderr,"Expected one of %s %s %s %s %s %s\n",
		    lookup(NONTERMINAL), lookup(TERMINAL), lookup(LEFTSQB),
		    lookup(LEFTCYB), lookup(LEFTPAR), lookup(NULLSTRING) );
	    exit(EXIT_FAILURE);
	    break;
    }
}

/*
	This is a "helper" routine which facilitates the processing of
	EBNF constructs by performing various housekeeping tasks such
	as setting up the new, internally generated production, ensuring that
	it has a unique name, remembering where we are in the current
	production and inserting any new productions necessary. When the
	processing of the productions inside the EBNF delimiters is done,
	the routine restores things so that we are working on the current 
	production again.
*/
void ProcessEBNF(int ebnf_form)
{
    int PreservedCurrentProduction;
    SYMBOL *InternalSymbol,
	   *InternalSymbol2;

    /*
	perform actions common to all EBNF types, generate a unique internal
	symbol and add it to the end of the current production. Then save the
	current production index.
    */
    InternalSymbol = GenerateInternalSymbol();
    AddSymboltoProduction(InternalSymbol);
    PreservedCurrentProduction = CurrentProduction;

    /* now perform processing unique to each type */
    switch ( ebnf_form )  {
	case  OPTION  :
	    AddNewProduction(InternalSymbol);   /* add the T1 --> \eps  */
	    AddSymboltoProduction(NullString);  /* production           */
	    /* now process everything between the [] as if it's a new   */
	    /* right hand side                                          */
	    ParseRightHandSide(InternalSymbol);
	    break;
	case  GROUPING  :
	    ParseRightHandSide(InternalSymbol);
	    break;
	case  REPETITION  :
	    AddNewProduction(InternalSymbol);   /* add the T1 --> \eps  */
	    AddSymboltoProduction(NullString);  /* production           */
	    /* add the T1 --> T2 T1 production                          */
	    InternalSymbol2 = GenerateInternalSymbol();
	    AddNewProduction(InternalSymbol);
	    AddSymboltoProduction(InternalSymbol2);
	    AddSymboltoProduction(InternalSymbol);
	    ParseRightHandSide(InternalSymbol2);
	    break;
	default :
	    fprintf(stderr,"panic, unrecognised ebnf_form %d\n",ebnf_form);
	    exit(EXIT_FAILURE);
	    break;
    }
    CurrentProduction = PreservedCurrentProduction;
}

/*
	Routine to absorb the current token and get a new one from the
	input stream given that the current token is what is expected.
*/
void Accept(int ExpectedToken)
{
    if ( CurrentToken == ExpectedToken )  NextToken();
    else  SyntaxError( ExpectedToken, CurrentToken );
}

/*
	Routine to get the next token from the input stream,
	unconditionally.
*/
void NextToken(void)
{
    CurrentToken = yylex();
}

/*
	Report a syntax error caused by the expected and current tokens
	not matching in "Accept", then exit.
*/
void SyntaxError(int ExpectedToken, int ActualToken)
{
    printf( "Syntax Error: expected %s, got %s\n", lookup( ExpectedToken ),
	    lookup( ActualToken ) );
    printf( "token string \"%s\"\n", yytext );
    exit( EXIT_FAILURE );
}

/*
	Look up the preferred string representation for each token type
	and return a pointer to that string.
*/
char *lookup(int token)
{
    char *s;
    switch ( token )  {
	case  NONTERMINAL    :  s = "nonterminal";                 break;
	case  TERMINAL       :  s = "terminal";                    break;
	case  EQUIVALANCE    :  s = "-->";                         break;
	case  LEFTSQB        :  s = "[";                           break;
	case  RIGHTSQB       :  s = "]";                           break;
	case  LEFTPAR        :  s = "(";                           break;
	case  RIGHTPAR       :  s = ")";                           break;
	case  LEFTCYB        :  s = "{";                           break;
	case  RIGHTCYB       :  s = "}";                           break;
	case  SEMICOLON      :  s = ";";                           break;
	case  NULLSTRING     :  s = "\\eps";                       break;
	case  ENDINPUTSYM    :  s = "$";                           break;
	case  ENDOFFILE      :  s = "<end of file>";               break;
	case  INTERNALSYMBOL :  s = "internally generated symbol"; break;
	default       :  s = "illegal input"; break;
    }
    return s;
}

/*
	Search the symbol table for a symbol, if it is there return a
	pointer to it. If not, insert it and then return a pointer.

	The type of the symbol (i.e., TERMINAL, NONTERMINAL, etc) is
	passed as parameter. The symbol string is expected to be found in
	"yytext" as a null terminated array of characters. This is where
	the scanner normally puts the string of any token, but if you want
	to force a symbol into the table which hasn't been read by the
	scanner, this string has to be manually set up.
*/
SYMBOL *InsertSymbol(int symboltype,char *symbolstring)
{
    SYMBOL *s;
    char   *String;
    int hashindex;

    String = AddString(symbolstring);
    if ( NULL == ( s = Probe( String, &hashindex ) ) )  {
	s = EnterSymbol(String,hashindex);
	s->class = symboltype;
	PreserveString();
    }
    return s;
}

/*
	Start a new production in the internal table. This is done
	by hunting for an empty slot for a production (i.e., one with
	productions[i][0] == NULL). The production is given as left 
	hand side the value passed as parameter. The internal
	pointers are set up so that the next symbol to be added is
	added to the right hand side of the production selected
	by this routine.

*/
void AddNewProduction(SYMBOL *ProductionName)
{
    int i;

    for ( i = 0; i < MAXPRODUCTIONS && productions[i][0] != NULL; i++ ) ;
    if ( i >= MAXPRODUCTIONS )  {
	fprintf(stderr,"Sorry, too many productions, max %d\n",
		MAXPRODUCTIONS);
	exit( EXIT_FAILURE );
    }
    CurrentProduction = i;
    productions[i][0] = ProductionName;
    CurrentSymbol[i] = 1;
}

/*
	Dump the internal BNF production table. Production 0 is the
	"wrapper" production, designed to enclose the grammar in a shell
	containing an end of input symbol, so it is not shown. 

	Note -- this routine assumes there are no gaps in the production table,
	so if Optimize has been run, then RemoveGaps must also have been
	applied to the table (after Optimize).
*/
void ShowProductions(void)
{
    SYMBOL *s;
    int i, j;
    
    for ( i = 1; i < MAXPRODUCTIONS; i++ )  {
	if ( productions[i][0] == NULL )  break;
	printf( "%2d  ", i ); 
	for ( j = 0; j < CurrentSymbol[i] ; j++ )  {
	    s = productions[i][j];
	    switch ( s->class )  {
		case  TERMINAL:
		case  NONTERMINAL:
		case  INTERNALSYMBOL:  printf( "%s ", s->s );  break;
		default:  printf( "%s ", lookup(s->class));    break;
	    }
	    if ( j == 0 )  printf( "--> " );
	}
	putchar('\n');
    }
}

/*
	Add a new symbol to the end of the currently active production.
*/
void AddSymboltoProduction(SYMBOL *Symbol)
{
    int i;

    i = CurrentSymbol[CurrentProduction];
    if ( i >= MAXSYMBOLSPERPROD )  {
	fprintf(stderr,
		"Sorry, too many symbols in current production, max %d\n",
		MAXSYMBOLSPERPROD);
	exit( EXIT_FAILURE );
    }
    productions[CurrentProduction][i] = Symbol;
    i++;
    CurrentSymbol[CurrentProduction] = i;
}

/*
	Generate a unique internal symbol and put it in the symbol table.
	Return a pointer to it.
*/
SYMBOL *GenerateInternalSymbol(void)
{
    static int InternalSymbolNumber = 0;
    char   buffer[256];

    sprintf(buffer,INTERNALNAME,InternalSymbolNumber);
    InternalSymbolNumber++;
    return  InsertSymbol(INTERNALSYMBOL,buffer);
}

/*
	Dump the contents of the internal BNF grammar table in hex format.
	For debugging only. Note -- only the first 8 entires from each row
	are dumped, as this is all that will fit on a line
*/
void Dump(void)
{
    int i, j;
    printf( "CurrentProduction = %d\n", CurrentProduction );
    for ( i = 0; i <= CurrentProduction; i++ )  {
	for ( j = 0; j < 8 ; j++ )  {
	    printf("%08lx ", (long) productions[i][j] );
	}
	putchar('\n');
    }
}

/*
	This routine tries to optimise the internal BNF grammar if
	this is possible. In particular, it performs the following
	optimisation

	The EBNF to BNF conversion in ProcessEBNF acts as follows

		A --> ... B { C .... E } F ....

	becomes:

		A  --> .... B I1 F ...
		I1 --> \epsilon
		I1 --> I2 I1
		I2 --> C .... E
		I2 -->
		.
		.

	This "double indirection" is needed in the most general case
	because the production sequence within the repetition brackets
	may be arbitrarily complicated and so require more than one
	BNF prodction to represent it. Since this cannot be determined
	a priori by the recursive descent method, the double indirection
	is needed.

	However, in most cases the double indirection, while correct,
	is long-winded because the object within the repetition operator
	brackets is simple, i.e.,

		A --> a {a}

	using the full scheme this is expanded to:

		A  --> a I1
		I1 --> \epsilon
		I1 --> I2 I1
		I2 --> a

	which could be replaced by the more terse:

		A  --> a I1
		I1 --> \epsilon
		I1 --> a I1

	The strategy of this optimization routine is to look for
	productions of the form

		I1 --> \epsilon
		I1 --> I2 I1
		I2 --> A ... Z

	where there is only one production with I2 as its left hand
	side, and replace the structure with

		I1 --> \epsilon
		I1 --> A ... Z I1

	This optimization is only allowed on internally generated 
	productions -- we don't want to optimize away productions
	deliberately placed in the grammar by the user.
*/
void Optimize(void)
{
    int i, j, k, l;
    SYMBOL *s;

    /* search for an Ix --> Iy Ix production */
    for ( i = 1; i < MAXPRODUCTIONS; i++ )  {
	if ( productions[i][0] != NULL && 
	     productions[i][0]->class == INTERNALSYMBOL &&
	     CurrentSymbol[i] == 3 &&
	     productions[i][1]->class == INTERNALSYMBOL &&
	     productions[i][0] == productions[i][2] )  {  /* found one ! */
	    /* now search for a single Iy --> ... production */
	    s = productions[i][1];  k = 0;
	    for ( j = 1; j < MAXPRODUCTIONS; j++ )  {
		if ( s == productions[j][0] )  {
		    l = j;  k++;
		}
	    }
	    /* if k == 1 we have a candidate for removal, indexed by l  */
	    if ( k == 1 )  {
		for ( j = 1; j < CurrentSymbol[l]; j++ )
		    productions[i][j] = productions[l][j];
		if ( j < MAXSYMBOLSPERPROD )
		    productions[i][j] = productions[i][0];
		else  {
		    fprintf(stderr,"Optimize -- production too long\n");
		    exit(EXIT_FAILURE);
		}
		CurrentSymbol[i] = j+1;
		/* now remove the Iy production                         */
		CurrentSymbol[l] = 0;
		productions[l][0] = NULL;
	    }
	}
    }
}

/*
	RemoveGaps

	Removes any gaps in the productions table which may have been left
	by the optimisation process. These are identified by having 
	CurrentSymbol[i] == 0, productions[i][0] == NULL,
	productions[i][1] |= NULL.
*/

void RemoveGaps(void)
{
    int i, j, k;

    for ( i = 0; i < MAXPRODUCTIONS; i++ )  {
	if ( CurrentSymbol[i] == 0 && productions[i][0] == NULL &&
	     productions[i][1] != NULL )  {
	    for ( j = i; j < MAXPRODUCTIONS-1; j++ )  {
		CurrentSymbol[j] = CurrentSymbol[j+1];
		for ( k = 0; k < MAXSYMBOLSPERPROD; k++ )
		    productions[j][k] = productions[j+1][k];
	    }
	    CurrentSymbol[MAXPRODUCTIONS-1] = 0;
	    for ( k = 0; k < MAXSYMBOLSPERPROD; k++ )
		productions[MAXPRODUCTIONS-1][k] = NULL;
	}
    }
}
