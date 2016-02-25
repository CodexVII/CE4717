/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       parser1                                                            */
/*                                                                          */
/*       Author: Ian Lodovica (13131567)                                    */
/*                                                                          */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       An illustration of the use of the character handler and scanner    */
/*       in a parser for the language                                       */
/*                                                                          */
/*                                                                          */
/*       Although the listing file generator has to be initialised in       */
/*       this program, full listing files cannot be generated in the        */
/*       presence of errors because of the "crash and burn" error-          */
/*       handling policy adopted. Only the first error is reported, the     */
/*       remainder of the input is simply copied to the output (using       */
/*       the routine "ReadToEndOfFile") without further comment.            */
/*                                                                          */
/*--------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "global.h"
#include "scanner.h"
#include "line.h"


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
PRIVATE void ReadToEndOfFile( void );

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
        ParseProgram();
        fclose( InputFile );
        fclose( ListFile );
	printf("valid\n");
        return  EXIT_SUCCESS;
    }
    else 
        return EXIT_FAILURE;
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

    if( CurrentToken.code == VAR ){
      ParseDeclarations();
    }
    
    while( CurrentToken.code == PROCEDURE ){
      ParseProcDeclaration();
    }
    
    while( CurrentToken.code == IDENTIFIER )  {
        ParseStatement();
        Accept( SEMICOLON );
    }

    ParseBlock();
    Accept( ENDOFPROGRAM );     /* Token "." has name ENDOFPROGRAM          */
    Accept( ENDOFINPUT );
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

  if( CurrentToken.code == VAR ){
    ParseDeclarations();
  }
  
  while( CurrentToken.code == PROCEDURE ){
    ParseProcDeclaration();
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
  
  while( CurrentToken.code == IDENTIFIER || CurrentToken.code == WHILE || CurrentToken.code == IF
      || CurrentToken.code == READ || CurrentToken.code == WRITE ){
    ParseStatement();
    Accept( SEMICOLON );
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
PRIVATE void ParseSubTerm(){
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
PRIVATE void ParseMultOp( void ){
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
/*           If the expected token fails to match the current lookahead,    */
/*           this routine reports a syntax error and exits ("crash & burn"  */
/*           parsing).  Note the use of routine "SyntaxError"               */
/*           (from "scanner.h") which puts the error message on the         */
/*           standard output and on the listing file, and the helper        */
/*           "ReadToEndOfFile" which just ensures that the listing file is  */
/*           completely generated.                                          */
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
    if ( CurrentToken.code != ExpectedToken )  {
        SyntaxError( ExpectedToken, CurrentToken );
        ReadToEndOfFile();
        fclose( InputFile );
        fclose( ListFile );
        exit( EXIT_FAILURE );
    }
    else  CurrentToken = GetToken();
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


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ReadToEndOfFile:  Reads all remaining tokens from the input file.       */
/*              associated input and listing files.                         */
/*                                                                          */
/*    This is used to ensure that the listing file refects the entire       */
/*    input, even after a syntax error (because of crash & burn parsing,    */
/*    if a routine like this is not used, the listing file will not be      */
/*    complete.  Note that this routine also reports in the listing file    */
/*    exactly where the parsing stopped.  Note that this routine is         */
/*    superfluous in a parser that performs error-recovery.                 */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Reads all remaining tokens from the input.  There won't */
/*                  be any more available input after this routine returns. */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ReadToEndOfFile( void )
{
    if ( CurrentToken.code != ENDOFINPUT )  {
        Error( "Parsing ends here in this program\n", CurrentToken.pos );
        while ( CurrentToken.code != ENDOFINPUT )  CurrentToken = GetToken();
    }
}
