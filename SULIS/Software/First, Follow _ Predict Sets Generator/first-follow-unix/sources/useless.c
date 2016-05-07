/*
	Source file for the routine FlagUselessProductions which checks
	a grammar for useless productions, i.e., those which either

	(1)	cannot be reached from the start symbol
	(2)	do not terminate in a string of terminals
*/
#include <stdio.h>
#define  USELESS	 1
#include "global.h"
#include "ff.h"
#include "useless.h"
#include "symbol.h"

#define  DERIVES_TERMINALS   		1  
#define  DOESNT_DERIVE_TERMINALS	2
#define  REACHED         		3
#define  NOTREACHED         		4

/*----------------------------------------------------------------------*/
/*									*/
/*	Global variables						*/
/*	(see parser.c where these are defined)				*/
/*									*/
/*----------------------------------------------------------------------*/

extern SYMBOL *productions[MAXPRODUCTIONS][MAXSYMBOLSPERPROD];
extern int    CurrentSymbol[MAXPRODUCTIONS];

extern SYMBOL *NullString;

/*----------------------------------------------------------------------*/

/*
	Flag 2 types of useless production --
		(1)  Unreachable productions 
		(2)  Non-Terminating productions

	Unreachable productions are those which cannot be reached from the
	start symbol. We check to see if there are any such productions by
	keeping an array of flags indicating those productions which have
	been reached from the start symbol. Beginning with the start symbol
	we mark all those productions reached from it in 1 iteration, 2
	iterations, 3 iterations, etc, until no more changes can be made to
	the flag array. When this point is achieved, the unreachable 
	productions are availible by inspection of the flag array.


	Non-terminating productions are those which never reduce to a
	string of terminal symbols, instead cycling through a never-ending
	set of non-terminal expansions. These are detected by a similar  
	technique to that used to find unreachable productions. An array of
	flags is kept, one for each production. If the left-hand-side of the
	production can be replaced at any stage by a string which we know to be
	composed of terminals only, we set a flag in the array to indicate
	this. The process iterates until no changes can be made in the
	flag array, at which point all the non-terminating productions
	can be identified.

	If useless prodctions are identified in the source, return 1, 
	otherwise return 0.

*/
int FlagUselessProductions(void)
{
    int  i, j, k, changes, 
	 production_derives_terminals, 
	 nonterminal_derives_terminals, 
	 found_useless,
	 flags[MAXPRODUCTIONS];
    SYMBOL *s;

    /* mark all but the start symbol as unreached initially, the 
       start symbol is always reached
    */
    *flags = REACHED;	
    for ( i = 1; i < MAXPRODUCTIONS; i++ ) *(flags+i) = NOTREACHED;

    /* find all the unreachable productions				*/
    do  {
	changes = 0;
	for ( i = 0; i < MAXPRODUCTIONS; i++ )  {
	    if ( productions[i][0] != NULL && flags[i] == REACHED )  {
		for ( j = 1; j < CurrentSymbol[i]; j++ )  {
		    s = productions[i][j];
		    if (s->class==NONTERMINAL || s->class==INTERNALSYMBOL)  {
			for ( k = 1; k < MAXPRODUCTIONS; k++ )  {
			    if ( s == productions[k][0] )  {
				if ( flags[k] == NOTREACHED )  {
				    changes |= 1;
				    flags[k] = REACHED;
				}
			    }
			}
		    }
		}
	    }
	}
    }  while ( changes );

    /* now find all the nonterminating productions			*/

    for ( i = 0; i < MAXPRODUCTIONS; i++ )  
	if ( flags[i] == REACHED )  flags[i] = DOESNT_DERIVE_TERMINALS;

    do  {
	changes = 0;
	for ( i = 1; i < MAXPRODUCTIONS; i++ )  {
	    if ( productions[i][0] != NULL && flags[i] != NOTREACHED )  {
		production_derives_terminals = DERIVES_TERMINALS;
		for ( j = 1; j < CurrentSymbol[i]; j++ )  {
		    s = productions[i][j];
		    if ( s->class == NONTERMINAL || 
			 s->class == INTERNALSYMBOL )  {
			nonterminal_derives_terminals = 0;
			for ( k = 1; k < MAXPRODUCTIONS; k++ )  {
			    if ( productions[k][0] == s ) 
				nonterminal_derives_terminals |= flags[k];
			}
			production_derives_terminals &=
						nonterminal_derives_terminals;
		    }
		}
		if ( production_derives_terminals && 
		     flags[i] != DERIVES_TERMINALS )  {
		    flags[i] = DERIVES_TERMINALS;
		    changes |= 1;
		}
	    }
	}
    }  while ( changes );

    /* report on the scan and return the appropriate flag. */
    found_useless = 0;

    for ( j = i = 1; i < MAXPRODUCTIONS; i++ )  {
	if ( productions[i][0] != NULL )  {
	    if ( flags[i] == DOESNT_DERIVE_TERMINALS || 
		 flags[i] == NOTREACHED )  {
		found_useless = 1;
		printf( "WARNING: Production (%1d) is useless ", j );
		if ( flags[i] == NOTREACHED )  printf( "(unreachable)\n" );
		else  printf( "(non-terminating)\n" );
	    }
	    j++;
	}
    }
    return found_useless;
}
