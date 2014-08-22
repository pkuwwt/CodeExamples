/**
   @file      fitsUtil.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     Deal with cfitsio errors@EOL
   @Keywords  cfitsio errors
   @Std       C89

              Error handling
*/

#include <stdio.h>              /* I/O lib         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */
#include "fitsio.h"

void reportAndExitOnFITSerror(int status) {
  if(status) {
    fits_report_error(stderr, status);
    exit(1);
  }
}
