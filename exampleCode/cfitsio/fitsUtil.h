/**
   @file      fitsUtil.h
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     Deal with cfitsio errors@EOL
   @Keywords  include cfitsio errors
   @Std       C89

              Error handling
*/

void reportAndExitOnFITSerror(int status);
