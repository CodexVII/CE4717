#ifndef  SYMBOLHEADER
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*      symbol.h                                                             */
/*                                                                           */
/*      Header file for "symbol.c", containing constant declarations,        */
/*      type definitions and function prototypes for the symbol table        */
/*      routines.                                                            */
/*                                                                           */
/*---------------------------------------------------------------------------*/

#define  SYMBOLHEADER

#include "global.h"

#define  HASHSIZE       997     /* Should be a prime for efficient hashing.  */
#define  MAXHASHLENGTH  100     /* Maximum number of characters taken into   */
                                /* account in a string when hashing.         */
typedef struct symboltype  {
    char *s;                    /* character string name of symbol           */
    int  scope;                 /* scope level of symbol                     */
    int  class;                 /* type of symbol                            */
    int  index;			/* index into first and follow set arrays    */
				/* for non terminals (use - ff.h)            */
    struct symboltype *next;    /* pointer to next symbol in chain           */
}
    SYMBOL;

PUBLIC SYMBOL *Probe(char *String, int *hashindex);
PUBLIC SYMBOL *EnterSymbol(char *String, int hashindex);
PUBLIC void    DumpSymbols(int scope);
PUBLIC void    RemoveSymbols(int scope);

#endif

