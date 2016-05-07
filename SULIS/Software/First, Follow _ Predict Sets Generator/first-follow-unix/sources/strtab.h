#ifndef  STRINGTABLEHEADER
/*---------------------------------------------------------------------------*/
/*                                                                           */
/*      strtab.h                                                             */
/*                                                                           */
/*      Header file for "strtab.c", containing constant declarations,        */
/*      type definitions and function prototypes for the string table        */
/*      routines.                                                            */
/*                                                                           */
/*---------------------------------------------------------------------------*/

#define  STRINGTABLEHEADER

#include "global.h"

PUBLIC char *AddString(char *String);
PUBLIC char *GetString(void);
PUBLIC void  PreserveString(void);
#endif
