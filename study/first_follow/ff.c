/*
    ff: first, follow and prdict set generator.  
*/
#include <stdio.h>
#include <stdlib.h>
#define  FF         1
#include "global.h"
#include "ff.h"
#include "parser.h"
#include "symbol.h"
#include "strtab.h"
#include "useless.h"

#define  SETSIZE	100	/* max elements per first/follow set	*/


/*----------------------------------------------------------------------*/
/*									*/
/*		Global variables                         	 	*/
/*									*/
/*----------------------------------------------------------------------*/

/*
	definition of the SET type
*/
typedef struct {
    SYMBOL *name;	/* name of production to which set belongs	*/
    int    elements;	/* number of elements currently in set		*/
    SYMBOL *e[SETSIZE];	/* array of elements				*/
}
    SET;
    
SET first[MAXPRODUCTIONS],	/* first sets for each production	*/
    firstnt[MAXPRODUCTIONS],	/* first sets for each nonterminal	*/
    follow[MAXPRODUCTIONS],	/* follow sets for each nonterminal	*/
    predict[MAXPRODUCTIONS];	/* predict sets for each production	*/

int NonTerminals = 0;		/* number of unique nonterminals	*/
int Productions = 0;		/* number of unique productions		*/

/*
	see parser.c for a discussion of the use of these variables
*/
extern SYMBOL *productions[MAXPRODUCTIONS][MAXSYMBOLSPERPROD];
extern int    CurrentSymbol[MAXPRODUCTIONS];

extern SYMBOL *NullString;

/*----------------------------------------------------------------------*/
/*									*/
/*		Function prototypes for internal routines		*/
/*									*/
/*----------------------------------------------------------------------*/

void ClearSet(SET *set);
int  Intersection(SET *set1, SET *set2, SET *intersection);
int  Union(SET *set1, SET *set2);
void DeleteElement(SET *destset, SET *srcset, SYMBOL *symbol);
int  Element(SET *set, SYMBOL *symbol);
int  AddtoSet(SET *set, SYMBOL *symbol);
void ReportLL1Conflicts( void );
void GeneratePredictSets( void );
void GenerateFollowSets( void );
int  ComputeFirst(SET *set, int ProdIndex, int SymIndex);
void GenerateFirstSets( void );
void DisplaySets(SET *setarray, int setnumber);
void EstablishSets( void );


/*----------------------------------------------------------------------*/

int main( void )
{
    ParseInput();
    if ( FlagUselessProductions() ) {
	printf("Useless productions found: analysis halted.\n");
	return -1;
    }
    EstablishSets();
    GenerateFirstSets();
    GenerateFollowSets();
    GeneratePredictSets();
    printf("First Sets\n" );  DisplaySets(first,Productions);
    printf("First Sets of nonterminals\n" );
    DisplaySets(firstnt,NonTerminals);
    printf("Follow Sets\n" ); DisplaySets(follow,NonTerminals);
    printf("Predict Sets\n"); DisplaySets(predict,Productions);
    ReportLL1Conflicts();
    return 0;
}

void EstablishSets( void )
{
    int i, j, notfoundbefore;

    Productions = NonTerminals = 0;
    for ( i = 0; i < MAXPRODUCTIONS; i++ )  {
	if ( productions[i][0] != NULL )  {
	    ClearSet(first+i);		/* first sets are indexed by	*/
	                                /* production number, not name	*/
	    ClearSet(predict+i);	/* ditto for predict sets	*/
	    Productions++;
	    for ( notfoundbefore = 1, j = 0; j < NonTerminals; j++ )  {
		if ( firstnt[j].name == productions[i][0] )  {
		    notfoundbefore = 0;  break;
		}
	    }
	    if ( notfoundbefore )  {
		firstnt[NonTerminals].name = productions[i][0];
		firstnt[NonTerminals].elements = 0;
		productions[i][0]->index = NonTerminals;
		follow[NonTerminals].name = productions[i][0];
		follow[NonTerminals].elements = 0;
		NonTerminals++;
	    }
	}
    }
}

void DisplaySets(SET *setarray, int setnumber)
{
    int i, index, j;

    /* If the set to be displayed is the FIRST set of the productions
       we need to index the display by production number and also check
       for "empty" production slots which may have been placed in the
       list bby optimzation routines.
       The fact that we are looking at the first set of productions can
       be deduced by looking at the name field of the set, if this is a
       null pointer then we are looking at the production first set
    */
    if ( setarray[0].name == NULL )  { 
	for ( i = index = 1; i < MAXPRODUCTIONS; i++ )  {
	    if ( productions[i][0] != NULL )  {
		printf( "(%1d)\t\t{ ", index );  index++;
		for ( j = 0; j < setarray[i].elements; j++ )  {
		    printf( "%s", setarray[i].e[j]->s );
		    if ( j < setarray[i].elements - 1 )  printf( ", " );
		    else  putchar( ' ' );
		}
		printf("}\n");
	    }
	}
    }
    /* Otherwise we must be looking at a first or follow set of 
       nonterminals. This is easier to deal with. Note that the first
       set of the list (index 0) is never displayed, as it is the
       set (either first or follow) belonging to the "wrapper" production
       which is a dummy added to ensure that the follow sets of the
       real productions cannot contain \epsilon.
    */
    else  {
	for ( i = 1; i < setnumber; i++ )  {
	    printf( "%.14s  { ", setarray[i].name->s );
	    for ( j = 0; j < setarray[i].elements; j++ )  {
		printf( "%s", setarray[i].e[j]->s );
		if ( j < setarray[i].elements - 1 )  printf( ", " );
		else  putchar( ' ' );
	    }
	    printf("}\n");
	}
    }
}

void GenerateFirstSets( void )
{
    int i, j, changes;

    /* insert all trivial (1 step production) values into the FIRST
       set of non terminal symbols
    */
    /* fprintf(stderr,"Entered GenerateFirstSets\n"); fflush(stderr); */

    for ( i = 0; i < NonTerminals; i++ )
	for ( j = 0; j < MAXPRODUCTIONS; j++ ) 
	    if ( productions[j][0] != NULL && 
		 productions[j][0] == firstnt[i].name &&
		 productions[j][1]->class != NONTERMINAL &&
		 productions[j][1]->class != INTERNALSYMBOL )
		AddtoSet(firstnt+i,productions[j][1]);
    /* now build the complete FIRST sets for all non terminals
    */
    do {
	changes = 0;
	for ( i = 0; i < NonTerminals; i++ )
	    for ( j = 0; j < MAXPRODUCTIONS; j++ ) 
		if ( productions[j][0] != NULL && 
		     productions[j][0] == firstnt[i].name &&
		     ( productions[j][1]->class == NONTERMINAL ||
		       productions[j][1]->class == INTERNALSYMBOL ) )
		    changes |= ComputeFirst(firstnt+i,j,1);
    }
    while ( changes );
    /* finally build the first sets for all productions */
    do {
	changes = 0;
	for ( i = 0; i < MAXPRODUCTIONS; i++ )
	    if ( productions[i][0] != NULL )  
		changes |= ComputeFirst(first+i,i,1);
    }
    while ( changes );

    /* fprintf(stderr,"Exiting GenerateFirstSets\n"); fflush(stderr); */
}

int ComputeFirst(SET *set, int ProdIndex, int SymIndex)
{
    int i, epsilon, changes;
    SET tempset, *s;

    /* fprintf(stderr,"Entering ComputeFirst, ProdIndex = %d, SymIndex = %d\n",
                   ProdIndex, SymIndex); fflush(stderr);
		   */

    epsilon = 1, changes = 0;
    for ( i = SymIndex; epsilon && i < CurrentSymbol[ProdIndex]; i++ )  {
	if ( productions[ProdIndex][i]->class == NONTERMINAL ||
	     productions[ProdIndex][i]->class == INTERNALSYMBOL )  {
	    ClearSet(&tempset);
	    s = firstnt+(productions[ProdIndex][i]->index);
	    if ( Element(s,NullString) )  {
		DeleteElement(&tempset,s,NullString);
		changes |= Union(set,&tempset);
	    }  
	    else  {
		epsilon = 0;
		changes |= Union(set,s);
	    }
	}
	else  {
	    changes |= AddtoSet(set,productions[ProdIndex][i]);
	    epsilon = 0;
	}
    }
    if ( epsilon )  changes |= AddtoSet(set,NullString);

    /* fprintf(stderr,"Exiting ComputeFirst, changes = %d\n", changes); 
       fflush(stderr); */

    return changes;

}


void GenerateFollowSets( void )
{
    int i, j, changes;
    SET tempset1, tempset2;

    /* fprintf(stderr,"Entering GenerateFollowSets\n"); fflush(stderr); */
    do {
	changes = 0;
	for ( i = 0; i < MAXPRODUCTIONS; i++ )  {
	    if ( productions[i][0] != NULL )  {
	      /* fprintf(stderr,"Working with production %d\n", i);
	         fflush(stderr); */

		for ( j = 1; j < CurrentSymbol[i]; j++ )  {
		    ClearSet(&tempset1);  ClearSet(&tempset2);
		    ComputeFirst(&tempset1,i,j+1);
		    if ( Element(&tempset1,NullString) )  {
			DeleteElement(&tempset2,&tempset1,NullString);

			/* fprintf(stderr," productions[%d][%d]->index = %d\n",
				   i, j, productions[i][j]->index);
			   fflush(stderr); */

			changes |= Union(follow+(productions[i][j]->index),
					 &tempset2);

			/* fprintf(stderr," productions[%d][%d]->index = %d\n",
				   i, 0, productions[i][0]->index);
			   fflush(stderr); */

			changes |= Union(follow+(productions[i][j]->index),
			                 follow+(productions[i][0]->index));
		    }
		    else  {
 		        /* fprintf(stderr," productions[%d][%d]->index = %d\n",
				   i, j, productions[i][j]->index);
			   fflush(stderr); */

			changes |= Union(follow+(productions[i][j]->index),
					 &tempset1);
		    }
		}
	    }
	}
    }  while ( changes );
    /* fprintf(stderr,"Exiting GenerateFollowSets\n"); fflush(stderr); */
}

void GeneratePredictSets( void )
{
    int i;

    /* fprintf(stderr,"Entering GeneratePredictSets\n"); fflush(stderr); */
    for ( i = 1; i < MAXPRODUCTIONS; i++ )  {
	if ( productions[i][0] != NULL )  {
	    if ( Element(first+i,NullString) )  {
		DeleteElement(predict+i,first+i,NullString);
		Union(predict+i,follow+(productions[i][0]->index));
	    }
	    else  Union(predict+i,first+i);
	}
    }
    /* fprintf(stderr,"Exiting GeneratePredictSets\n"); fflush(stderr); */
}

void ReportLL1Conflicts( void )
{
    int i, j, conflict,
	production_number[MAXPRODUCTIONS],
	checked_already[MAXPRODUCTIONS],
	conflicting_productions[MAXPRODUCTIONS];
    SET union_of_intersections,   /* Build up set of conficting elements from clashing productions */
        pairwise_intersection;    /* This holds a particular conflict set for one pariwise
                                     intersection between sets with a common left-hand-side. */

    /* fprintf(stderr,"Entering ReportLL1Conflicts\n"); fflush(stderr); */

    for ( j = i = 1; i < MAXPRODUCTIONS; i++ )  {
	*(checked_already+i) = 0;
	if ( productions[i][0] != NULL )  {
	    production_number[i] = j;  j++;
	}
	else  production_number[i] = -1;
    }
    conflict = 0;
    for ( i = 1; i < MAXPRODUCTIONS; i++ )  {
	if ( productions[i][0] != NULL && !checked_already[i] )  {
	    ClearSet(&union_of_intersections);
	    checked_already[i] = 1;
	    for ( j = 1; j < MAXPRODUCTIONS; j++ )
		*(conflicting_productions+j) = 0;
	    for ( j = 1; j < MAXPRODUCTIONS; j++ )  {
		ClearSet(&pairwise_intersection);
		if ( i != j && !checked_already[j] &&
		     productions[i][0] == productions[j][0] )  {
		    checked_already[j] = 1;
		    conflict |= Intersection(predict+i,predict+j,&pairwise_intersection);
		    if ( pairwise_intersection.elements > 0 )  {
			Union(&union_of_intersections,&pairwise_intersection);
			conflicting_productions[i] = 1;
			conflicting_productions[j] = 1;
		    }
		}
	    }
	    if ( union_of_intersections.elements > 0 )  {
		printf("LL1 predict conflict\n    predict sets " );
		for ( j = 1; j < MAXPRODUCTIONS; j++ )
		    if ( conflicting_productions[j] )
			printf( "(%1d) ", production_number[j] );
		printf("in conflict\n    union of pairwise intersection sets { " );
		for ( j = 0; j < union_of_intersections.elements; j++ )  {
		    printf("%s", union_of_intersections.e[j]->s );
		    if ( j == union_of_intersections.elements - 1 )  printf(" }\n");
		    else  printf(", ");
		}
	    }
	}
    }
    if ( conflict )  printf("Grammar is not LL1\n");
    else  printf("Grammar is LL1\n");

    /* fprintf(stderr,"Exiting ReportLL1Conflicts\n"); fflush(stderr); */
}

int AddtoSet(SET *set, SYMBOL *symbol)
{
    int i, notfound;

    if ( symbol == NULL ) {
      fprintf(stderr,"SYMBOL IS NULL\n");
      fflush(stderr);
      exit(EXIT_FAILURE);
    }

    /*
    fprintf(stderr,"Adding %s to ", symbol->s);
    if ( set->name == NULL )  fprintf(stderr,"a first set\n");
    else  {
      if ( set->name == NULL )
	fprintf(stderr,"a set whose name is NULL\n");
      else
	fprintf(stderr,"set %s\n",set->name->s);
    }
    fflush(stderr);
    */

    notfound = 1;
    for ( i = 0; i < SETSIZE && i < set->elements; i++ )  {
	if ( set->e[i] == symbol )  {
	    notfound = 0;
	    break;
	}
    }
    if ( notfound )  {
	if ( i >= SETSIZE )  {
	    fprintf(stderr,"error - too many elements in set\n");
	    exit(EXIT_FAILURE);
	}
	else  {
	    set->elements++;  set->e[i] = symbol;
	}
    }
    return notfound;
}

int Element(SET *set, SYMBOL *symbol)
{
    int i, found;

    found = 0;
    for ( i = 0; i < SETSIZE && i < set->elements; i++ )  {
	if ( (found = ( set->e[i] == symbol )) != 0 )  break;
    }
    return  found;
}

void DeleteElement(SET *destset, SET *srcset, SYMBOL *symbol)
{
    int i, j;

    for ( i = j = 0; i < SETSIZE && i < srcset->elements; i++ )  {
	if ( srcset->e[i] != symbol )  {
	    destset->e[j] = srcset->e[i];  j++;
	}
    }
    destset->elements = j;
    destset->name = srcset->name;
}

int Union(SET *set1, SET *set2)	/* set1 <-- set1 union set2 */
{
    int i, set1changed;

    /* fprintf(stderr,"Entering Union, set1 = %08x, set2 = %08x\n",
	       (int) set1, (int) set2 );
       fflush(stderr);
    */

    if ( set1 == NULL ) {
      fprintf(stderr,"Error, set1 == NULL\n"); fflush(stderr);
      exit(EXIT_FAILURE);
    }
    if ( set2 == NULL ) {
      fprintf(stderr,"Error, set2 == NULL\n"); fflush(stderr);
      exit(EXIT_FAILURE);
    }

    set1changed = 0;
    for ( i = 0; i < set2->elements; i++ )  
	set1changed |= AddtoSet(set1,set2->e[i]);
    return set1changed;
}

int Intersection(SET *set1, SET *set2, SET *intersection)  
                                       /* intersection <-- set1 ^ set2 */
{
    int i, j, foundintersection;

    foundintersection = 0;
    ClearSet(intersection);
    for ( i = 0; i < set1->elements; i++ )
	for ( j = 0; j < set2->elements; j++ )
	    if ( set1->e[i] == set2->e[j] )  {
		foundintersection = 1;
		AddtoSet(intersection,set1->e[i]);
	    }

    return  foundintersection;
}

void ClearSet(SET *set)
{
    set->name = NULL;  set->elements = 0;
}
