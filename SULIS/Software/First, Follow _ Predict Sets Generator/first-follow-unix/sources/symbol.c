/*---------------------------------------------------------------------------*/
/*                                                                           */
/*      symbol.c                                                             */
/*                                                                           */
/*      Implementation file for the symbol table.                            */
/*                                                                           */
/*      The symbol table is implemented as a hash table with a prime         */
/*      number of entries. Each entry is a pointer to a SYMBOL structure.    */
/*      Entries which have the same hash value form a chain of structures    */
/*      from the index entry, linked by the "next" field of the SYMBOL       */
/*      structure. New entries are placed at the head of the chain.          */
/*                                                                           */ 
/*---------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "symbol.h"

/*---------------------------------------------------------------------------*/
/*                                                                           */
/*      Definitions of constants local to the module                         */
/*                                                                           */
/*---------------------------------------------------------------------------*/

#define  MAXDISPLAYLENGTH                20   /* see DisplaySymbol           */
#define  MAX_SYMBOLS_TO_DISPLAY         100   /* see DumpSymbols             */

/*---------------------------------------------------------------------------*/
/*                                                                           */
/*      Function Prototypes for routines PRIVATE to this module              */
/*                                                                           */
/*---------------------------------------------------------------------------*/

PRIVATE int   Hash( char *String );
PRIVATE void  BubbleSort(SYMBOL *list[], int total_elements);
PRIVATE void  DisplaySymbol(SYMBOL *s);

/*---------------------------------------------------------------------------*/
/*                                                                           */
/*      Data Structures for this module                                      */
/*                                                                           */
/*---------------------------------------------------------------------------*/

PRIVATE SYMBOL *HashTable[HASHSIZE];    /* The hash table                    */

/*---------------------------------------------------------------------------*/
/*                                                                           */
/*      Public routines (globally accessable).                               */
/*                                                                           */
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*      Probe                                                                */
/*                                                                           */
/*      Searches the symbol table for the first instance of a particular     */
/*      string.                                                              */
/*                                                                           */
/*      Input(s):                                                            */
/*                                                                           */
/*          String     pointer to a null terminated character string         */
/*                     which is to be searched for in the table.             */
/*                                                                           */
/*      Output(s):                                                           */
/*                                                                           */
/*          hashindex  pointer to an integer into which the hash value       */
/*                     of the string will be placed by this routine. If      */
/*                     the pointer is NULL, it is ignored.                   */
/*                                                                           */
/*      Returns:                                                             */
/*                                                                           */
/*          Pointer to the symbol holding the string. If no entry is found   */
/*          to match the string argument, NULL is returned.                  */
/*                                                                           */
/*      N.B.                                                                 */
/*                                                                           */
/*          As the hash function does not guarantee a unique hash index for  */
/*          every unique string, it is necessary to perform a string         */
/*          comparison between the target string and that found in the hash  */
/*          table, and follow the linked list of hash table pointers until a */
/*          matching string is found or NULL is encountered. Since there is  */
/*          a large number of hash buckets, the number of comparisons will   */
/*          be small on average, so the efficiency of this symbol table is   */
/*          high.                                                            */
/*                                                                           */
/*---------------------------------------------------------------------------*/

PUBLIC SYMBOL *Probe(char *String, int *hashindex)
{
    int hash;
    SYMBOL *symptr;

    hash = Hash( String );
    symptr = *(HashTable+hash);
    while ( symptr != NULL && 0 != strcmp( symptr->s, String ) )
        symptr = symptr->next;
    if ( hashindex != NULL )  *hashindex = hash;
    return  symptr;
}

/*---------------------------------------------------------------------------*/
/*                                                                           */
/*      EnterSymbol                                                          */
/*                                                                           */
/*      Inserts a new symbol structure into the symbol table containing      */
/*      the string "String". The fields of the SYMBOL are initialised as     */
/*      follows:                                                             */
/*                                                                           */
/*          s          = String                                              */
/*          scope      = -1                                                  */
/*          class      = -1                                                  */
/*          next       = pointer to chain of other SYMBOLs with the same     */
/*                       hash index. The symbol just inseted into the hash   */
/*                       table becomes the new head of this chain.           */
/*                                                                           */
/*      Input(s):                                                            */
/*                                                                           */
/*          String     pointer to a null terminated character string         */
/*                     which is to be placed in the table.                   */
/*                                                                           */
/*          hashindex  pointer to an integer containing the hash index       */
/*                     where the SYMBOL is to be placed.                     */
/*                                                                           */
/*      Output(s):     None                                                  */
/*                                                                           */
/*      Returns:                                                             */
/*                                                                           */
/*          Pointer to the symbol holding the string. If no memory can be    */
/*          allocated for the SYMBOL, returns NULL.                          */
/*                                                                           */
/*---------------------------------------------------------------------------*/

PUBLIC SYMBOL *EnterSymbol(char *String, int hashindex)
{
    SYMBOL *symptr;

    if ( NULL != ( symptr = (SYMBOL *) malloc( sizeof(SYMBOL) ) ) )  {
        symptr->s = String;
        symptr->scope = -1;
        symptr->class = -1;
	symptr->index = 0;
        symptr->next = *(HashTable+hashindex);
        *(HashTable+hashindex) = symptr;
    }
    return  symptr;
}

/*---------------------------------------------------------------------------*/
/*                                                                           */
/*      DumpSymbols                                                          */
/*                                                                           */
/*      Sort all the symbols at a scope level greater than or equal to the   */
/*      "scope" parameter and display them on "stdout", typically the        */
/*      terminal.                                                            */
/*      "stdout".                                                            */
/*                                                                           */
/*      Input(s):                                                            */
/*                                                                           */
/*          scope      integer, the scope level which will determine what    */
/*                     symbols are to be displayed.                          */
/*                                                                           */
/*      Output(s):     None                                                  */
/*                                                                           */
/*      Returns:       Nothing                                               */
/*                                                                           */
/*      N.B.  The constant MAX_SYMBOLS_TO_DISPLAY determines the maximum     */
/*      number of symbols which can be displayed by this routine.            */
/*                                                                           */
/*---------------------------------------------------------------------------*/

PUBLIC void DumpSymbols(int scope)
{
    SYMBOL  *symptr, *list[MAX_SYMBOLS_TO_DISPLAY];
    int     i, j;

    for ( j = i = 0; i < HASHSIZE && j < MAX_SYMBOLS_TO_DISPLAY; i++ )  {
        symptr = *(HashTable+i);
        while( symptr != NULL && symptr->scope >= scope )  {
            if ( j >= MAX_SYMBOLS_TO_DISPLAY )  break;
            else  {
                *(list+j) = symptr;  j++;
                symptr = symptr->next;
            }
        }
    }

    BubbleSort( list, j );
    printf( "           name          " );
    printf( "| scope | class\n" );
    for ( i = 0; i < j; i++ )  {
        printf( "%3d: ", i );
        DisplaySymbol( *(list+i) ); 
        putchar( '\n' );
    }
}

/*---------------------------------------------------------------------------*/
/*                                                                           */
/*      RemoveSymbols                                                        */
/*                                                                           */
/*      Remove all the symbols whose "scope" field is greater than or equal  */
/*      to the parameter "scope" from the symbol table.                      */
/*                                                                           */
/*      Input(s):                                                            */
/*                                                                           */
/*          scope      integer, the scope level which will determine what    */
/*                     symbols are to be removed.                            */
/*                                                                           */
/*      Output(s):     None                                                  */
/*                                                                           */
/*      Returns:       Nothing                                               */
/*                                                                           */
/*---------------------------------------------------------------------------*/

PUBLIC void   RemoveSymbols(int scope)
{
    SYMBOL  *symptr, *temp;
    int     i;

    for ( i = 0; i < HASHSIZE; i++ )  {
        symptr = *(HashTable+i);
        while( symptr != NULL && symptr->scope >= scope )  {
            temp = symptr;
            symptr = symptr->next;
            free( temp );
        }
        *(HashTable+i) = symptr;
    }
}

/*---------------------------------------------------------------------------*/
/*                                                                           */
/*      Private routies (only accessable from within this module)            */
/*                                                                           */
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*      Hash                                                                 */
/*                                                                           */
/*      Generates a hash value for the string passed as a parameter.         */
/*                                                                           */
/*      Input(s):                                                            */
/*                                                                           */
/*          String     pointer to a null terminated character string         */
/*                     which is used to generate the hash value.             */
/*                                                                           */
/*      Output(s):     None                                                  */
/*                                                                           */
/*      Returns:       Hash value (an integer).                              */
/*                                                                           */
/*      N.B.           The length of the string used to generate the         */
/*      hash index is limited by the constant MAXHASHLENGTH. This is to      */
/*      prevent absurd behaviour in the case that the string is not          */
/*      properly terminated.                                                 */
/*                                                                           */
/*---------------------------------------------------------------------------*/

PRIVATE int   Hash( char *String )
{
    int hashvalue, i;
    char *p;

    for ( p = String, hashvalue = i = 0; i < MAXHASHLENGTH; p++, i++ )  {
        if ( *p != '\0' )  hashvalue += (int) *p & 0x7f;
        else break;
    }
    hashvalue = hashvalue % HASHSIZE;
    return hashvalue;
}

/*---------------------------------------------------------------------------*/
/*                                                                           */
/*      BubbleSort                                                           */
/*                                                                           */
/*      Bubble sorts into ascending lexicographic order an array of symbols  */
/*      passed as a parameter.                                               */ 
/*                                                                           */
/*      Input/Output:                                                        */
/*                                                                           */
/*          list       An array of pointers to SYMBOLS. As input this list   */
/*                     us unsorted, it is returned sorted as output.         */
/*                                                                           */
/*      Inputs(s):                                                           */
/*                                                                           */
/*          total_elements  The total number of elements in the list         */
/*                                                                           */
/*      Returns:       Nothing                                               */
/*                                                                           */
/*---------------------------------------------------------------------------*/

PRIVATE void  BubbleSort(SYMBOL *list[], int total_elements)
{
    int swapped, i;
    SYMBOL *temp;

    do  {
        swapped = 0;
        for ( i = 0; i < total_elements - 1; i++ )  {
            if ( strcmp( list[i]->s, list[i+1]->s ) > 0 )  {
                temp = list[i];
                list[i] = list[i+1];
                list[i+1] = temp;
                swapped = 1;
            }
        }
    }
    while ( swapped );
}

/*---------------------------------------------------------------------------*/
/*                                                                           */
/*      DisplaySymbol                                                        */
/*                                                                           */
/*      Displays the name and attributes of a symbol on the standard         */
/*      output device.                                                       */
/*                                                                           */
/*      Input(s):                                                            */
/*                                                                           */
/*          s          Pointer to the symbol to be displayed.                */
/*                                                                           */
/*      Output(s):     None                                                  */
/*                                                                           */
/*      Returns:       Nothing                                               */
/*                                                                           */
/*---------------------------------------------------------------------------*/

PRIVATE void  DisplaySymbol(SYMBOL *s)
{
    int i;
    char *cp;
    
    for ( cp = s->s, i = 0; i < MAXDISPLAYLENGTH; i++ )  {
        if ( *cp == '\0' )  putchar( ' ' );  
        else  {
            putchar( *cp );  cp++;
        }
    }
    printf( "|  %3d  |  %3d  ", s->scope, s->class );
}
