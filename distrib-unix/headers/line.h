#ifndef  LINEHEADER
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*      line.h                                                               */
/*                                                                           */
/*      Header file for "line.c", containing constant declarations, type     */
/*      definitions and function prototypes for the character processor.     */
/*                                                                           */
/*---------------------------------------------------------------------------*/

#define  LINEHEADER

#include <stdio.h>
#include "global.h"

#define  M_LINE_WIDTH           74              /* maximum line width        */
#define  M_ERRS_LINE             5              /* max displayed errors per  */
                                                /* line                      */

PUBLIC void   InitCharProcessor( FILE *inputfile, FILE *listfile );
PUBLIC int    ReadChar( void );
PUBLIC void   UnReadChar( void );
PUBLIC int    CurrentCharPos( void );
PUBLIC void   Error( char *ErrorString, int PositionInLine );
PUBLIC void   SetTabWidth( int NewTabWidth );
PUBLIC int    GetTabWidth( void );

#endif
