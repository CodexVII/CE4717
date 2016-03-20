/*                                                                          */
/*                                                                          */
/*       Author: Ian Lodovica (13131567)                                    */
/*                                                                          */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       Builds on top of parser2. Includes basic code generation without   */
/*       taking procedures or local variables into account                  */
/*                                                                          */
/*--------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "global.h"
#include "scanner.h"
#include "line.h"
#include "sets.h"
#include "symbol.h"
#include "strtab.h"
#include "code.h"

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Global variables used by this parser.                                   */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE FILE *InputFile;           /*  CPL source comes from here.          */
PRIVATE FILE *ListFile;            /*  For nicely-formatted syntax errors.  */
PRIVATE FILE *CodeFile;		   /*  Assemly code */

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
PRIVATE int scope = 1;		   /*  Contains scope of variables          */
                                   /*  not too concenred with it in comp1   */
PRIVATE int varaddress;		   /*  Used inside MakeSymboleTableEntry    */


int prec[256];			   /* Table of operator precedences.        */
int operatorInstruction[256];      /* Complementary table to prec           */
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
PRIVATE void ParseRestOfStatement( SYMBOL *target ); /* changed from void */
PRIVATE void ParseProcCallList( void );
PRIVATE void ParseActualParameter( void );
PRIVATE void ParseTerm( void );
PRIVATE void ParseSubTerm( void );
PRIVATE void ParseAssignment( void );
PRIVATE void ParseWhileStatement( void );
PRIVATE int ParseBooleanExpression( void ); /* now returns int */
PRIVATE int ParseRelOp( void );		    /* now returns int */
PRIVATE void ParseIfStatement( void );
PRIVATE void ParseReadStatement( void );
PRIVATE void ParseWriteStatement( void );
PRIVATE void Synchronise( SET *F, SET *FB );
PRIVATE void SetupSets( void );
PRIVATE void MakeSymbolTableEntry( int symtype );
PRIVATE SYMBOL *LookupSymbol( void );
PRIVATE void ParseOpPrec( int minPrec );
PRIVATE void SetupOpPrecTables( void );

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Main: comp1 entry point.                                                */
/*                                                                          */
/*      Initializes files that are passed in as arguments. Sets up          */
/*      tables and sets. Calls to parse the program and then write          */
/*      the accompanying assembly code.                                     */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PUBLIC int main ( int argc, char *argv[] )
{
    if ( OpenFiles( argc, argv ) )  {
        InitCharProcessor( InputFile, ListFile );
	InitCodeGenerator( CodeFile );
	SetupOpPrecTables();
        CurrentToken = GetToken();
	SetupSets();
        ParseProgram();
	DumpSymbols(scope);
	_Emit(I_HALT);
	WriteCodeFile();
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
/*  SetupOpPrecTables: Sets up the tables used when comparing different     */
/*               operatator precedences. Used for RPN parsing.              */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: prec and operatorInstruction arrays initialized         */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE void SetupOpPrecTables( void )
{
  int i;

  for( i = 0; i < 256; i++){
    prec[i] = -1;
  }

  prec[MULTIPLY] = 20;  
  prec[DIVIDE]   = 20;  
  prec[ADD]      = 10;  
  prec[SUBTRACT] = 10;  

  operatorInstruction[MULTIPLY] = I_MULT;
  operatorInstruction[DIVIDE]   = I_DIV;
  operatorInstruction[ADD]      = I_ADD;
  operatorInstruction[SUBTRACT] = I_SUB;
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseOpPrec: Implements operator precedence parsing in a recursive      */
/*               way. Used when parsing expressions. Makes sure that        */
/*               the load order and operator use is correct when generating */
/*               code.                                                      */
/*                                                                          */
/*    Inputs:       int minPrec - the minimum precedence that this level of */
/*                                ParseOpRec will accept                    */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token can be moved forward several times      */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseOpPrec( int minPrec )
{
  int op1, op2;

  op1 = CurrentToken.code;
  while( prec[op1] >= minPrec ){
    CurrentToken = GetToken();
    ParseTerm();
    op2 = CurrentToken.code;
    if( prec[op2] > prec[op1] ){
      ParseOpPrec( prec[op1] +1 );
    }
    _Emit( operatorInstruction[op1] );
    op1 = CurrentToken.code;
  }
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  MakeSymbolEntry: Adds an identifier to the symbol table. Checks if      */
/*               the symbol has already been declared previously.           */
/*               Calculates and assigns addresses in memory for variables   */
/*               and procedures.                                            */
/*                                                                          */
/*    Inputs:       int symtype - used to set the type of the identifier    */
/*                                when adding to the symbol table.          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Symbol table may be modified.                           */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE void MakeSymbolTableEntry( int symtype )
{
  SYMBOL *oldsptr = NULL;
  char *cptr = NULL;
  SYMBOL *newsptr = NULL;
  int hashindex;

    if ( CurrentToken.code == IDENTIFIER ) {
      if ( NULL == ( oldsptr = Probe( CurrentToken.s, &hashindex )) || oldsptr->scope < scope ) {
	if ( oldsptr == NULL ){
	  cptr = CurrentToken.s;
	} else {
	  cptr = oldsptr->s;
	}
	if ( NULL == ( newsptr = EnterSymbol( cptr, hashindex ))) {
	  Error( "Internal EnterSymbol error. Must exit", CurrentToken.pos );
	} else {
	  if ( oldsptr == NULL ){
	    PreserveString();
	  }
	  newsptr->scope = scope;
	  newsptr->type = symtype;
	  if ( symtype == STYPE_VARIABLE ){
	    newsptr->address = varaddress;
	    varaddress++;
	  } else {
	    newsptr->address = -1;
	  }
	}
      } else {
	Error( "Variable or Procedure already declared", CurrentToken.pos );
	ParseStatus = 1;
      }
    } 
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  LookupSymbol: Checks to see if the identifier has been previously       */
/*                declared. Normal procedure before an identifier is        */
/*                accepted during code generation.                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      SYMBOL *sptr - identifier as found in SymbolTable       */
/*                                                                          */
/*    Side Effects: None                                      */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE SYMBOL *LookupSymbol( void )
{
  SYMBOL *sptr;
  
  if( CurrentToken.code == IDENTIFIER ){
    sptr = Probe( CurrentToken.s, NULL );
    if( sptr == NULL ){
      Error( "Identifier not declared", CurrentToken.pos );
      KillCodeGeneration();
      ParseStatus = 1;
    }
  } else {
    sptr = NULL;
  }

  return sptr;
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
  MakeSymbolTableEntry( STYPE_PROCEDURE );
  Accept( IDENTIFIER );
  scope++;

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
  RemoveSymbols( scope );
  scope--;
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
  while( InSet(&StatementFS, CurrentToken.code) ){
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
  int var_count = 1; 		/* at least 1 global if this func is called */

  Accept( VAR );
  MakeSymbolTableEntry( STYPE_VARIABLE );
  Accept( IDENTIFIER );
  
  while( CurrentToken.code == COMMA ){
    Accept( COMMA );
    MakeSymbolTableEntry( STYPE_VARIABLE );
    Accept( IDENTIFIER );
    var_count++;
  }
  
  Accept( SEMICOLON );
  
  /* Increment memory space by amount of global variables declared */
  Emit(I_INC, var_count);
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
  _Emit( I_WRITE );  
  while( CurrentToken.code == COMMA ){
    Accept( COMMA );
    ParseExpression();
    _Emit( I_WRITE );
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
  SYMBOL *target;

  Accept( READ );
  Accept( LEFTPARENTHESIS );
  target = LookupSymbol();  	/* Verify identifier has been declared */
  Accept( IDENTIFIER );

  if( target != NULL && target->type == STYPE_VARIABLE ){
    _Emit( I_READ );
    Emit( I_STOREA, target->address ); 
  }else{
    Error( "Undeclared Variable", CurrentToken.pos );
  }
  
  while( CurrentToken.code == COMMA ){
    Accept( COMMA );
    target = LookupSymbol();
    Accept( IDENTIFIER );

    if( target != NULL && target->type == STYPE_VARIABLE ){
      _Emit( I_READ );
      Emit( I_STOREA, target->address ); 
    }else{
      Error( "Undeclared Variable", CurrentToken.pos );
    }
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
  int Label1, Label2, L1BackPatchLoc, L2BackPatchLoc;

  Accept( IF );
  L1BackPatchLoc = ParseBooleanExpression();
  Accept( THEN );
  ParseBlock();

  /* Retain current address to allow code to skip the ELSE block
     Branch skip placed in a seperate for loop to accommodate for
     IF-THEN and IF-THEN-ELSE structures
  */
  if( CurrentToken.code == ELSE ){
    L2BackPatchLoc = CurrentCodeAddress();
    Emit( I_BR, 0 );
  }
  
  /* Forces the program to skip to the ELSE statement if the expression
     in the if statement returned false
   */
  Label1 = CurrentCodeAddress();
  BackPatch( L1BackPatchLoc, Label1 );
  
  
  if( CurrentToken.code == ELSE ){
    Accept( ELSE );
    ParseBlock();
    Label2 = CurrentCodeAddress();
    BackPatch( L2BackPatchLoc, Label2 );
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
  SYMBOL *target;
  target = LookupSymbol();  	/* Verify identifier has been declared */

  Accept( IDENTIFIER );
  ParseRestOfStatement( target );
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
  int Label1, Label2, L2BackPatchLoc;

  Accept( WHILE );
  Label1 = CurrentCodeAddress();
  L2BackPatchLoc = ParseBooleanExpression();
  Accept( DO );
  ParseBlock();
  Emit( I_BR, Label1 );
  Label2 = CurrentCodeAddress();
  BackPatch( L2BackPatchLoc, Label2 );
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
PRIVATE int ParseBooleanExpression( void )
{
  int BackPatchAddr, RelOpInstruction;

  ParseExpression();
  RelOpInstruction = ParseRelOp();
  ParseExpression();
  _Emit( I_SUB );
  BackPatchAddr = CurrentCodeAddress();
  Emit( RelOpInstruction, 0 );
  return BackPatchAddr;
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
PRIVATE int ParseRelOp( void )
{
  int RelOpInstruction;

  switch( CurrentToken.code ){
  case EQUALITY:
    RelOpInstruction = I_BZ;
    Accept( EQUALITY );
    break;
  case LESSEQUAL:
    RelOpInstruction = I_BG;
    Accept( LESSEQUAL );
    break;
  case GREATEREQUAL:
    RelOpInstruction = I_BL;
    Accept( GREATEREQUAL );
    break;
  case LESS:
    Accept( LESS );
    RelOpInstruction = I_BGZ;
    break;
  case GREATER:
  default:
    RelOpInstruction = I_BLZ;
    Accept( GREATER );
  }
  return RelOpInstruction;
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseRestOfStatement implements                                       */
/*                                                                          */
/*      <RestOfStatement>  :== <ProcCallList> | <Assignment> | null         */
/*                                                                          */
/*                                                                          */
/*    Inputs:       SYMBOL *target - Identifier that's either a procedure   */
/*                                   or variable.                           */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*--------------------------------------------------------------------------*/
PRIVATE void ParseRestOfStatement( SYMBOL *target ) /* ParseRestOfStatement( SYMBOL *target ) */
{
  switch( CurrentToken.code )
  {
  case LEFTPARENTHESIS:
    ParseProcCallList(); 	/* ProcCallList( target ) */
  case SEMICOLON:
    if( target != NULL && target->type == STYPE_PROCEDURE ){
      Emit( I_CALL, target->address );
    } else {
      Error( "Not a procedure", CurrentToken.pos );
      KillCodeGeneration();
    }
    break;
  case ASSIGNMENT:
  default: 
    ParseAssignment();
    if( target != NULL && target->type == STYPE_VARIABLE ){
      Emit( I_STOREA, target->address );
    }else{
      Error( "Undelared Variable", CurrentToken.pos );
      KillCodeGeneration();
    }
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
  ParseExpression();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*    ParseExpression implements                                            */
/*                                                                          */
/*      <ParseExpression>  :== <CompoundTerm> {<AddOp> <CompoundTerm>}      */
/*                                                                          */
/*      **This has been updated to forgo explicitly parsing <CompoundTerm>, */
/*        <AddOp> and subsequently <MultOp> in favor of <OpPrec>**          */
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
  ParseTerm();
  ParseOpPrec( 0 );
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
  int negate_flag = 0;

  if( CurrentToken.code == SUBTRACT){
    negate_flag = 1;
    Accept( SUBTRACT );
  }
  ParseSubTerm();

  if( negate_flag ){
    _Emit( I_NEG );
  }
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
  SYMBOL *var;

  switch( CurrentToken.code){
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
    var = LookupSymbol();	/* checks if variable is declared */
    Accept( IDENTIFIER );
    if( var != NULL && var->type == STYPE_VARIABLE){
      Emit( I_LOADA, var->address );
    }else{
      Error( "Variable undeclared.", CurrentToken.pos );
      ParseStatus = 1;
    }
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

    recovering = 1;
    ParseStatus = 1;
  }else{
    CurrentToken = GetToken();
  }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  OpenFiles:  Reads strings from the command-line and opens the           */
/*              associated input, listing file and code file.               */
/*                                                                          */
/*    Note that this routine mmodifies the globals "InputFile",             */
/*    "ListingFile" and "CodeFile".  It returns 1 ("true" in C-speak) if    */
/*    the files are successfully opened, 0 if not, allowing the caller      */
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
/*                  "ListingFile" and "CodeFile"                             */
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
        fprintf( stderr, "cannot open \"%s\" for output\n", argv[3] );
        fclose( CodeFile );
        return 0;
    }

    return 1;
}

