/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       expression.c   Parser/Compiler for an Expression language,         */
/*                      converting an expression read from the input        */
/*                      file to assembly code for the stack machine.        */
/*                      very small (& modified) subset of the CPL grammar.  */
/*                                                                          */
/*       Shows how to get a compiler for expressions going.  Note that      */     
/*       these expressions can only contain integer constants.              */
/*                                                                          */
/*       <Expression>    :==  <CompoundTerm> { ("+"|"-") <CompoundTerm> }   */
/*       <CompoundTerm>  :==  <Term> { ("*"|"/") <Term> }                   */
/*       <Term>          :==  ["-"] <SubTerm>                               */
/*       <SubTerm>       :==  <IntConst> | "(" <Expression> ")"             */
/*                                                                          */
/*                                                                          */
/*       Note - <IntConst> is provided by the scanner as token INTCONST.    */
/*                                                                          */
/*--------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "global.h"
#include "scanner.h"
#include "line.h"
#include "code.h"


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Global variables used by this parser.                                   */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE FILE *InputFile;           /*  CPL source comes from here.          */
PRIVATE FILE *ListFile;            /*  For nicely-formatted syntax errors.  */
PRIVATE FILE *CodeFile;            /*  CPL stack-machine assembly goes here.*/

PRIVATE TOKEN  CurrentToken;       /*  Parser lookahead token.  Updated by  */
                                   /*  routine Accept (below).  Must be     */
                                   /*  initialised before parser starts.    */


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Function prototypes                                                     */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseExpression( void );
PRIVATE void ParseCompoundTerm( void );
PRIVATE void ParseTerm( void );
PRIVATE void ParseSubTerm( void );

PRIVATE int  OpenFiles( int argc, char *argv[] );
PRIVATE void Accept( int code );
PRIVATE void ReadToEndOfFile( void );


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Main: Program entry point.  Sets up parser globals (opens input and     */
/*        output files, initialises current lookahead), then calls          */
/*        "ParseProgram" to start the parse.                                */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PUBLIC int main ( int argc, char *argv[] )
{
    if ( OpenFiles( argc, argv ) )  {
        InitCharProcessor( InputFile, ListFile );
        InitCodeGenerator( CodeFile );
        CurrentToken = GetToken();
        ParseExpression();
        _Emit( I_WRITE );   /* Print out top of stack, i.e, result of exp.  */
        _Emit( I_HALT );    /* Stop simulator.                              */
        WriteCodeFile();
        fclose( InputFile );
        fclose( ListFile );
        return  EXIT_SUCCESS;
    }
    else 
        return EXIT_FAILURE;
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseExpression( void )
{
    int op;
    
    ParseCompoundTerm(); 
    while ((op = CurrentToken.code) == ADD || op == SUBTRACT ) {
        Accept( op );
        ParseCompoundTerm();
        if ( op == ADD ) _Emit( I_ADD ); else _Emit( I_SUB );
    }
}

PRIVATE void ParseCompoundTerm( void )
{
    int op;
    
    ParseTerm(); 
    while ((op = CurrentToken.code) == MULTIPLY || op == DIVIDE ) {
        Accept( op );
        ParseTerm();
        if ( op == MULTIPLY ) _Emit( I_MULT ); else _Emit( I_DIV );
    }
}

PRIVATE void ParseTerm( void )
{
    int negateflag = 0;
    
    if ( CurrentToken.code == SUBTRACT ) {
        negateflag = 1;  Accept( SUBTRACT );
    }
    ParseSubTerm();

    if ( negateflag ) _Emit( I_NEG );
}

PRIVATE void ParseSubTerm( void )
{
    switch ( CurrentToken.code ) {
        case INTCONST:
            Emit( I_LOADI, CurrentToken.value );
            Accept( INTCONST );
            break;
        case LEFTPARENTHESIS:
            Accept( LEFTPARENTHESIS );
            ParseExpression();
            Accept( RIGHTPARENTHESIS );
            break;
        default:
            Error("IntConst or ( expected here.", CurrentToken.pos);
            KillCodeGeneration();
            break;
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
/*              associated input, listing and code files.                   */
/*                                                                          */
/*    Note that this routine modifies the globals "InputFile",              */
/*    "ListingFile" and "CodeFile".  It returns 1 ("true" in C-speak) if    */
/*    all files are successfully opened, 0 if not, allowing the caller      */
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
/*    Side Effects: If successful, modifies globals "InputFile",            */
/*                  "ListingFile" and "CodeFile".                           */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE int  OpenFiles( int argc, char *argv[] )
{

    if ( argc != 4 )  {
        fprintf( stderr, "%s <inputfile> <listfile> <codefile>\n", argv[0] );
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

    if ( NULL == ( CodeFile = fopen( argv[3], "w" ) ) )  {
        fprintf( stderr, "cannot open \"%s\" for output\n", argv[2] );
        fclose( InputFile );
        fclose( ListFile );
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
