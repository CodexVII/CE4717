/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       parser2                                                            */
/*                                                                          */
/*       Author: Ian Lodovica (13131567)                                    */
/*                                                                          */
/*       Date: 25th of  February 2016                                       */
/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       An illustration of the use of the character handler and scanner    */
/*       in a parser for the language                                       */
/*                                                                          */
/*       Builds on top of parser1. Now implements basic error recovery      */
/*       which allows it to keep parsing finding more errors if they        */
/*       exist                                                              */
/*                                                                          */
/*                                                                          */
/*--------------------------------------------------------------------------*/

/*
  Error recovery with S-Algol and Pascal

  repition operator
  imbalance between BEGIN and END

  At EBNF points of the grammar, allow re-synchronization to happen
  at entry and exit points

  Work out SetupSets
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "global.h"
#include "scanner.h"
#include "line.h"
#include "sets.h"

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Global variables used by this parser.                                   */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE FILE *InputFile;           /*  CPL source comes from here.          */
PRIVATE FILE *ListFile;            /*  For nicely-formatted syntax errors.  */

PRIVATE TOKEN  CurrentToken;       /*  Parser lookahead token.  Updated by  */
                                   /*  routine Accept (below).  Must be     */
                                   /*  initialised before parser starts.    */

PRIVATE int ParseStatus;	   /*  Used for displaying the approriate   */
				   /*  message once parsing is complete     */
                                   /*  1 = Invalid, 0 = Valid */

PRIVATE SET DeclarationsFS_aug;    /*  SET structs used to contain sets     */
PRIVATE SET DeclarationsFBS;	   /*  needed to move from crash and burn   */
PRIVATE SET ProcDeclarationFS_aug; /*  parsing to error recovery            */
PRIVATE SET ProcDeclarationFBS;
PRIVATE SET StatementFS_aug;
PRIVATE SET StatementFBS;
PRIVATE SET StatementFS;           /*  Convinience set for block parsing    */

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Function prototypes                                                     */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE int  OpenFiles( int argc, char *argv[] );
PRIVATE void ParseProgram( void );
PRIVATE void ParseStatement( void );
PRIVATE void ParseExpression( void );
PRIVATE void Accept( int code );

PRIVATE void ParseDeclarations( void );
PRIVATE void ParseProcDeclaration( void );
PRIVATE void ParseBlock( void );
PRIVATE void ParseParameterList( void );
PRIVATE void ParseFormalParameter( void );
PRIVATE void ParseSimpleStatement( void );
PRIVATE void ParseRestOfStatement( void );
PRIVATE void ParseProcCallList( void );
PRIVATE void ParseActualParameter( void );
PRIVATE void ParseCompoundTerm( void );
PRIVATE void ParseAddOp( void );
PRIVATE void ParseTerm( void );
PRIVATE void ParseSubTerm( void );
PRIVATE void ParseMultOp( void );
PRIVATE void ParseAssignment( void );
PRIVATE void ParseWhileStatement( void );
PRIVATE void ParseBooleanExpression( void );
PRIVATE void ParseRelOp( void );
PRIVATE void ParseIfStatement( void );
PRIVATE void ParseReadStatement( void );
PRIVATE void ParseWriteStatement( void );
PRIVATE void Synchronise( SET *F, SET *FB );
PRIVATE void SetupSets( void );

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Main: Smallparser entry point.  Sets up parser globals (opens input and */
/*        output files, initialises current lookahead), then calls          */
/*        "ParseProgram" to start the parse.                                */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PUBLIC int main ( int argc, char *argv[] )
{
    if ( OpenFiles( argc, argv ) )  {
        InitCharProcessor( InputFile, ListFile );
        CurrentToken = GetToken();
	SetupSets();
        ParseProgram();
        fclose( InputFile );
        fclose( ListFile );
	if(ParseStatus != 0){
	  printf("invalid\n");
	}else{
	  printf("valid\n");
	}
        return  EXIT_SUCCESS;
    }
    else 
        return EXIT_FAILURE;
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Synchronise: Allows for continual parsing by skipping over sections of  */
/*               with the intent of stopping at in a location where parsing */
/*               can continue.                                              */
/*                                                                          */
/*    Inputs:       SET *F - Set that contains elements for the first set   */
/*                           of the production being parsed.                */
/*                  SET *FB - Follow and Beacon sets for the production     */
/*                            being parsed                                  */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token can be moved forward multiple times.    */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE void Synchronise(SET *F, SET *FB)
{
  SET S;

  S = Union( 2, F, FB );
  if( !InSet(F, CurrentToken.code) ){
    ParseStatus = 1;
    SyntaxError2( *F, CurrentToken );
    while( !InSet(&S, CurrentToken.code) ){
      CurrentToken = GetToken();
    }
  }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  SetupSets: Initializes set objects that will be used throughout the     */
/*             parsing. Such sets include the First, Follow and Beacon sets.*/
/*             The Augmented Follow sets are also included where necessary. */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: None.                                                   */
/*--------------------------------------------------------------------------*/
PRIVATE void SetupSets( void )
{
  InitSet( &DeclarationsFS_aug, 3, VAR, PROCEDURE, BEGIN );
  InitSet( &DeclarationsFBS, 3, ENDOFPROGRAM, ENDOFINPUT, END );
  InitSet( &ProcDeclarationFS_aug, 2, PROCEDURE, BEGIN );
  InitSet( &ProcDeclarationFBS, 3, ENDOFPROGRAM, ENDOFINPUT, END );
  InitSet( &StatementFS, 5, IDENTIFIER, WHILE, IF, READ, WRITE );
  InitSet( &StatementFS_aug, 6, IDENTIFIER, WHILE, IF, READ, WRITE, END );
  InitSet( &StatementFBS, 4, SEMICOLON, ELSE, ENDOFPROGRAM, ENDOFINPUT );
  InitSet( &StatementFS, 5, IDENTIFIER, WHILE, IF, READ, WRITE );
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseProgram implements:                                              */
/*                                                                          */
/*      <Program>  :== "PROGRAM" <Identifier> ";"                           */
/*                     [<Declarations>] {<ProcDeclaration>} <Block> "."     */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseProgram( void )
{
    Accept( PROGRAM );
    Accept( IDENTIFIER );
    Accept( SEMICOLON );

    Synchronise(&DeclarationsFS_aug, &DeclarationsFBS);
    if( CurrentToken.code == VAR ){
      ParseDeclarations();
    }
    
    Synchronise(&ProcDeclarationFS_aug, &ProcDeclarationFBS);
    while( CurrentToken.code == PROCEDURE ){
      ParseProcDeclaration();
      Synchronise(&ProcDeclarationFS_aug, &ProcDeclarationFBS);
    }

    ParseBlock();
    Accept( ENDOFPROGRAM );     /* Token "." has name ENDOFPROGRAM          */
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseProcDeclaration implements                                       */
/*                                                                          */
/*      <ProcDeclaration>  :== "PROCEDURE" <Identifier> [<ParameterList>]   */
/*                             ";" [<Declarations>] {<ProcDeclaration>}     */
/*                             <Block> ";"                                  */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseProcDeclaration( void )
{
  Accept( PROCEDURE );
  Accept( IDENTIFIER );
  
  if( CurrentToken.code == LEFTPARENTHESIS ){
    ParseParameterList();
  }
  Accept( SEMICOLON );

  Synchronise(&DeclarationsFS_aug, &DeclarationsFBS);
  if( CurrentToken.code == VAR ){
    ParseDeclarations();
  }
  
  Synchronise(&ProcDeclarationFS_aug, &ProcDeclarationFBS);
  while( CurrentToken.code == PROCEDURE ){
    ParseProcDeclaration();
    Synchronise(&ProcDeclarationFS_aug, &ProcDeclarationFBS);
  }
  
  ParseBlock();
  Accept( SEMICOLON );
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseParameterList implements                                         */
/*                                                                          */
/*      <ParameterList>  :== "(" <FormalParameter> {"," <FormalParameter>}  */
/*                           ")"                                            */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseParameterList( void )
{
  Accept( LEFTPARENTHESIS );  
  ParseFormalParameter();

  while( CurrentToken.code == COMMA ){
    Accept( COMMA );
    ParseFormalParameter();
  }
  
  Accept( RIGHTPARENTHESIS );
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseFormalParameter implements                                       */
/*                                                                          */
/*      <FormalParameter>  :== ["REF"] <Variable>                           */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseFormalParameter( void )
{
  if( CurrentToken.code == REF ){
    Accept( REF );
  }
  Accept( IDENTIFIER );
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseBlock implements                                                 */
/*                                                                          */
/*      <Block>  :== "BEGIN" {<Statement> ";"} "END"                        */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseBlock( void )
{
  Accept( BEGIN );
  
  Synchronise( &StatementFS_aug, &StatementFBS );
  while( InSet( &StatementFS, CurrentToken.code) ){
    ParseStatement();
    Accept( SEMICOLON );
    Synchronise( &StatementFS_aug, &StatementFBS );
  }

  Accept( END );
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseDeclarations implements                                          */
/*                                                                          */
/*      <Declarations>  :== "VAR" <Variable> {"," <Variable>} ";"           */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseDeclarations( void )
{
  Accept( VAR );
  Accept( IDENTIFIER );

  while( CurrentToken.code == COMMA ){
    Accept( COMMA );
    Accept( IDENTIFIER );
  }
  
  Accept( SEMICOLON );
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseStatement implements                                             */
/*                                                                          */
/*      <Statement>  :== <SimpleStatement> | <WhileStatement> |             */
/*                       <IfStatement> | <ReadStatement> | <riteStatement>  */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseStatement( void )
{
  if( CurrentToken.code == WHILE ){
    ParseWhileStatement();
  }else if( CurrentToken.code == IF ){
    ParseIfStatement();
  }else if( CurrentToken.code == READ ){
    ParseReadStatement();
  }else if( CurrentToken.code == WRITE ){
    ParseWriteStatement();
  }else{
    ParseSimpleStatement();
  }
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseWriteStatement implements                                        */
/*                                                                          */
/*      <WriteStatement>  :== "WRITE" "(" <Expression> {"," <Expression>}   */
/*                            ")"                                           */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseWriteStatement( void )
{
  Accept( WRITE );
  Accept( LEFTPARENTHESIS );
  
  ParseExpression();
  
  while( CurrentToken.code == COMMA ){
    Accept( COMMA );
    ParseExpression();
  }

  Accept( RIGHTPARENTHESIS );
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseReadStatement implements                                         */
/*                                                                          */
/*      <ReadStatement>  :== "READ" "(" <Variable> {"," <Variable>} ")"     */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseReadStatement( void )
{
  Accept( READ );
  Accept( LEFTPARENTHESIS );
  Accept( IDENTIFIER );

  while( CurrentToken.code == COMMA ){
    Accept( COMMA );
    Accept( IDENTIFIER );
  }
  
  Accept( RIGHTPARENTHESIS );
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseIfStatement implements                                           */
/*                                                                          */
/*      <IfStatement>  :== "IF" <BooleanExpression> "THEN" <Block>          */
/*                         ["ELSE" <Block>]                                 */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseIfStatement( void )
{
  Accept( IF );
  ParseBooleanExpression();
  Accept( THEN );
  ParseBlock();
  
  if( CurrentToken.code == ELSE ){
    Accept( ELSE );
    ParseBlock();
  }
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseSimpleStatement implements                                       */
/*                                                                          */
/*      <SimpleStatement>  :== <VarOrProcName> <RestOfStatement>            */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseSimpleStatement( void )
{
  Accept( IDENTIFIER );
  ParseRestOfStatement();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseWhileStatement implements                                        */
/*                                                                          */
/*      <WhileStatement>  :== "WHILE" <BooleanExpression> "DO" <Block>      */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseWhileStatement( void )
{
  Accept( WHILE );
  ParseBooleanExpression();
  Accept( DO );
  ParseBlock();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseBooleanExpression implements                                     */
/*                                                                          */
/*      <BooleanExpression> :== <Expression> <RelOp> <Expression>           */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseBooleanExpression( void )
{
  ParseExpression();
  ParseRelOp();
  ParseExpression();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseRelOp implements                                                 */
/*                                                                          */
/*      <RelOp>  :== "=" | "<=" | ">=" | "<" | ">"                          */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseRelOp( void )
{
  switch( CurrentToken.code ){
  case EQUALITY:
    Accept( EQUALITY );
    break;
  case LESSEQUAL:
    Accept( LESSEQUAL );
    break;
  case GREATEREQUAL:
    Accept( GREATEREQUAL );
    break;
  case LESS:
    Accept( LESS );
    break;
  case GREATER:
    Accept( GREATER );
    break;
  }
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseRestOfStatement implements                                       */
/*                                                                          */
/*      <RestOfStatement>  :== <ProcCallList> | <Assignment> | null         */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseRestOfStatement( void )
{
  if( CurrentToken.code == LEFTPARENTHESIS ){
    ParseProcCallList();
  }else if( CurrentToken.code == ASSIGNMENT ){
    ParseAssignment();
  }

  /* do nothing on null string */
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseProcCallList implements                                          */
/*                                                                          */
/*      <ProcCallList>  :== "(" <ActualParameter> {"," <ActualParameter>}   */
/*                          ")"                                             */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseProcCallList( void )
{
  Accept( LEFTPARENTHESIS );
  
  ParseActualParameter();

  while( CurrentToken.code == COMMA ){
    Accept( COMMA );
    ParseActualParameter();
  }

  Accept( RIGHTPARENTHESIS );
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseActualParemter implements                                        */
/*                                                                          */
/*      <ParseActualParameter>  :== <Variable> | <Expression>               */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseActualParameter( void )
{
  /* parse expression or identifier? 
     from how the language is setup an expression seems to be a superset
     of an identifier so we might be able to just always accept an expression
     here.
   */
  ParseExpression();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseExpression implements                                            */
/*                                                                          */
/*      <ParseExpression>  :== <CompoundTerm> {<AddOp> <CompoundTerm>}      */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseExpression( void )
{
  ParseCompoundTerm();
  while( CurrentToken.code == ADD || CurrentToken.code == SUBTRACT ){
    ParseAddOp();
    ParseCompoundTerm();
  }
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseCompoundTerm implements                                          */
/*                                                                          */
/*      <CompoundTerm>  :== <Term> {<MultOp> <Term>}                        */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseCompoundTerm( void )
{
  ParseTerm();
  
  while( CurrentToken.code == MULTIPLY || CurrentToken.code == DIVIDE ){
    ParseMultOp();
    ParseCompoundTerm();
  }
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseAssignment implements                                            */
/*                                                                          */
/*      <Assignment>  :== "=" <Expression>                                  */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseAssignment( void )
{
  Accept( ASSIGNMENT );
  ParseExpression();
}

/* runs fine even with this function's body  commented out */
/* a(-b) still considered legal by compiler */
/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseTerm implements                                                  */
/*                                                                          */
/*      <Term>  :== ["-"] <SubTerm>                                         */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseTerm( void )
{
  if( CurrentToken.code == SUBTRACT){
    Accept( SUBTRACT );
  }
  ParseSubTerm();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseSubTerm implements                                               */
/*                                                                          */
/*      <SubTerm>  :== <Variable> | <IntConst> | "(" <Expression ")"        */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseSubTerm( void ){
  if( CurrentToken.code == INTCONST ){
    Accept( INTCONST );
  }else if( CurrentToken.code == LEFTPARENTHESIS ){
    Accept( LEFTPARENTHESIS );
    ParseExpression();
    Accept( RIGHTPARENTHESIS );
  }else Accept( IDENTIFIER );
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseAddOp implements                                                 */
/*                                                                          */
/*      <AddOp>  :== "+" | "-"                                              */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseAddOp( void )
{
  if( CurrentToken.code == ADD ){
    Accept( ADD );
  }else{
    Accept( SUBTRACT );
  }
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseMultOp implements                                                */
/*                                                                          */
/*      <MultOp>  :== "*" | "/"                                             */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseMultOp( void )
{
  if( CurrentToken.code == MULTIPLY ){
    Accept( MULTIPLY );
  }else{
    Accept( DIVIDE );
  }
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  End of parser.  Support routines follow.                                */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Accept:  Takes an expected token name as argument, and if the current   */
/*           lookahead matches this, advances the lookahead and returns.    */
/*                                                                          */
/*                                                                          */
/*                                                                          */
/*    Inputs:       Integer code of expected token                          */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: If successful, advances the current lookahead token     */
/*                  "CurrentToken".                                         */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void Accept( int ExpectedToken )
{
  static int recovering = 0;
  
  if( recovering ){
    while( CurrentToken.code != ExpectedToken &&
	   CurrentToken.code != ENDOFINPUT ){
      CurrentToken = GetToken();
    }
    recovering = 0;
  }

  if( CurrentToken.code != ExpectedToken ){
    SyntaxError( ExpectedToken, CurrentToken );
    
    /* if EOF was seen an unexpected just give the syntax error and quit */
    /* no reason to continue */
    /* if( CurrentToken.code == ENDOFINPUT ){ */
    /*   exit( EXIT_FAILURE ); */
    /* } */
    recovering = 1;
    ParseStatus = 1;
  }else{
    CurrentToken = GetToken();
  }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  OpenFiles:  Reads strings from the command-line and opens the           */
/*              associated input and listing files.                         */
/*                                                                          */
/*    Note that this routine mmodifies the globals "InputFile" and          */
/*    "ListingFile".  It returns 1 ("true" in C-speak) if the input and     */
/*    listing files are successfully opened, 0 if not, allowing the caller  */
/*    to make a graceful exit if the opening process failed.                */
/*                                                                          */
/*                                                                          */
/*    Inputs:       1) Integer argument count (standard C "argc").          */
/*                  2) Array of pointers to C-strings containing arguments  */
/*                  (standard C "argv").                                    */
/*                                                                          */
/*    Outputs:      No direct outputs, but note side effects.               */
/*                                                                          */
/*    Returns:      Boolean success flag (i.e., an "int":  1 or 0)          */
/*                                                                          */
/*    Side Effects: If successful, modifies globals "InputFile" and         */
/*                  "ListingFile".                                          */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE int  OpenFiles( int argc, char *argv[] )
{

    if ( argc != 3 )  {
        fprintf( stderr, "%s <inputfile> <listfile>\n", argv[0] );
        return 0;
    }

    if ( NULL == ( InputFile = fopen( argv[1], "r" ) ) )  {
        fprintf( stderr, "cannot open \"%s\" for input\n", argv[1] );
        return 0;
    }

    if ( NULL == ( ListFile = fopen( argv[2], "w" ) ) )  {
        fprintf( stderr, "cannot open \"%s\" for output\n", argv[2] );
        fclose( InputFile );
        return 0;
    }

    return 1;
}

