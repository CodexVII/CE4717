/*--------------------------------------------------------------------------

        opprec.c

        Operator-precedence parser and code-generator for a simple grammar
        of expressions.

        Uses an operator-precedence parser to generate code for 
        expressions separated by semicolons.

 

--------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "global.h"
#include "scanner.h"
#include "strtab.h"
#include "symbol.h"
#include "line.h"
#include "sets.h"
#include "code.h"

/* FILEs for I/O, input (source), listing and assembly code.              */

PRIVATE FILE *InputFile;
PRIVATE FILE *ListFile;
PRIVATE FILE *CodeFile;

/* The parser lookahead token.  Global to the parser/compiler.            */

PRIVATE TOKEN  CurrentToken;

/* The table of operator precedences.  There is an entry for every possible
   TOKEN, most of them are initialised to -1, but the true operators will
   have precedences:

         MULTIPLY & DIVIDE    20
         ADD & SUBTRACT       10                                          */

int prec[256];    /* Note: Big enough to hold every possible TOKEN code.  */

/* The operator --> instruction mapping table.                            */

int operatorInstruction[256];   /* Same size as prec table.               */



/* Function Prototypes.                                                   */

PRIVATE void SetupOpPrecTables( void );
PRIVATE int  OpenFiles( int argc, char *argv[]);
PRIVATE void Accept( int code );


PRIVATE void ParseExpressions( void );
PRIVATE void ParseExpression( void );
PRIVATE void ParseOpPrec( int minPrec );
PRIVATE void ParseTerm( void );
PRIVATE void ParseSubTerm( void );


/* Entry point for parser.  main() sets up everything before calling     */
/* ParseExpressions().                                                   */

PUBLIC int main ( int argc, char *argv[] )
{
    if ( OpenFiles( argc, argv ) ) {
        SetupOpPrecTables();
        InitCharProcessor( InputFile, ListFile );
        InitCodeGenerator( CodeFile );
        CurrentToken = GetToken();
        ParseExpressions();
        WriteCodeFile();
        fclose( InputFile );
        fclose( ListFile );
        return  EXIT_SUCCESS;
    }
    return  EXIT_FAILURE;
}


/*---------------------------------------------------------------------------*/
/*                                                                           */
/*    Parser for expressions separated by semicolons.  Each expression is    */
/*    evaluated and printed using a WRITE instruction.   Note that it is     */
/*    possible to have an empty expression as the last expression (i.e. to   */
/*    terminate the final real expression by a semicolon).                   */       
/*                                                                           */
/*---------------------------------------------------------------------------*/

PRIVATE void ParseExpressions( void )
{
    ParseExpression(); _Emit( I_WRITE );
    while ( CurrentToken.code == SEMICOLON ) {
        Accept( SEMICOLON );
        if ( CurrentToken.code != ENDOFINPUT ) {  /* Anything "real" left?   */
            ParseExpression(); _Emit( I_WRITE );
        }
    }
    _Emit( I_HALT );  /* Halt should always be the last instruction 
                         generated. */
}


/*---------------------------------------------------------------------------*/
/*                                                                           */
/*    Operator Precedence Parser                                             */
/*                                                                           */
/*---------------------------------------------------------------------------*/

PRIVATE void ParseExpression( void )
{
    ParseTerm();
    ParseOpPrec( 0 );
}


PRIVATE void ParseOpPrec( int minPrec )
{
    int  op1, op2;

    op1 = CurrentToken.code;
    while ( prec[op1] >= minPrec )  {
        CurrentToken = GetToken();
        ParseTerm();
        op2 = CurrentToken.code;
        if ( prec[op2] > prec[op1] )  ParseOpPrec( prec[op1] + 1 );
        _Emit( operatorInstruction[op1] );
        op1 = CurrentToken.code;
    }
}


PRIVATE void ParseTerm( void )
{
    int negateflag = 0;

    if ( CurrentToken.code == SUBTRACT )  {
        negateflag = 1;  Accept( SUBTRACT );
    }
    ParseSubTerm();
    if ( negateflag )  _Emit( I_NEG );
}


PRIVATE void ParseSubTerm( void )
{
    switch ( CurrentToken.code )  {
        case  LEFTPARENTHESIS:  
            Accept( LEFTPARENTHESIS );
            ParseExpression();
            Accept( RIGHTPARENTHESIS );
            break;
        case  INTCONST:  
        default:
            Emit( I_LOADI, CurrentToken.value );
            Accept( INTCONST );
            break;
    }
}


/*---------------------------------------------------------------------------*/
/*                                                                           */
/*  Support Routines.                                                        */
/*                                                                           */
/*---------------------------------------------------------------------------*/

/*
        Initialise the operator precedence tables, both the precedence
        table 'prec' and the table mapping operators to instruction codes
        'operatorInstruction'.
*/

PRIVATE void SetupOpPrecTables( void )
{
    int i;

    for ( i = 0; i < 256; i++ ) prec[i] = -1;
    prec[MULTIPLY] = 20;  operatorInstruction[MULTIPLY] = I_MULT;
    prec[DIVIDE]   = 20;  operatorInstruction[DIVIDE]   = I_DIV;
    prec[ADD]      = 10;  operatorInstruction[ADD]      = I_ADD;
    prec[SUBTRACT] = 10;  operatorInstruction[SUBTRACT] = I_SUB; 
}


PRIVATE int OpenFiles( int argc, char *argv[] )
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
        fprintf( stderr, "cannot open \"%s\" for output\n", argv[3] );
        fclose( InputFile );
        fclose( ListFile );
        return 0;
    }

    return 1;
}


/*
        Accept using s-algol error recovery
*/

PRIVATE void Accept( int code )
{
    static int recovering = 0;

    if ( recovering )  {
        while ( CurrentToken.code != code && CurrentToken.code != ENDOFINPUT )
            CurrentToken = GetToken();
        if ( CurrentToken.code != ENDOFINPUT )
            Error( "parsing restarts here\n", CurrentToken.pos );
        recovering = 0;
    }

    if ( CurrentToken.code == code )  CurrentToken = GetToken();
    else   {
        SyntaxError( code, CurrentToken );
        KillCodeGeneration();   /* error => don't generate machine code */
        recovering = 1;
    }
}

