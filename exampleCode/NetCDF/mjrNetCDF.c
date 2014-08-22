/**
   @file      mjrNetCDF.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
   @brief     Simple functions to help with NetCDF programs.@EOL
   @Keywords  NetCDF
   @Std       C89
*/

#include <netcdf.h>             /* NetCDF Files          */
#include <stdio.h>              /* I/O lib         ISOC  */
#include <string.h>             /* Strings         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */

#include "mjrNetCDF.h"

/* Exit upon any nc_* error */
void mjrNC_chkError(int status) {
  if (status != NC_NOERR) {
    fprintf(stderr, "NC ERROR: %s\n", nc_strerror(status)); 
    exit(1); 
  } /* end if */
} /* end func mjrNC_chkError */


/* Wrapper for nc_put_att_text() that figures out the length of c-strings
   and makes sure the ending \0 will be stored in the string. */
int mjrNC_putAttCStr(int ncid, int varid, char *attName, char *attVal) {
  return nc_put_att_text(ncid, varid, attName,  strlen(attVal)+1, attVal);
} /* end func mjrNC_putAttCStr */


/* Convert an nc_type into a string. */
void mjrNC_type2str(nc_type ncXType, char *ncXTypeStr) {
  switch(ncXType) {
      case NC_BYTE:   strcpy(ncXTypeStr, "byte");   break;
      case NC_CHAR:   strcpy(ncXTypeStr, "char");   break;
      case NC_SHORT:  strcpy(ncXTypeStr, "short");  break;
      case NC_INT:    strcpy(ncXTypeStr, "int");    break;
      case NC_FLOAT:  strcpy(ncXTypeStr, "float");  break;
      case NC_DOUBLE: strcpy(ncXTypeStr, "double"); break;
      default:        strcpy(ncXTypeStr, "");       break;
  } /* end switch */
} /* end func mjrNC_type2str */

