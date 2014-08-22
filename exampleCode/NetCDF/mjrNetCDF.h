/**
   @file      mjrNetCDF.h
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
   @brief     Header file for some simple NetCDF functions.@EOL
   @Keywords  NetCDF
   @Std       C89
*/

#ifndef INC_MJRNETCDF
#define INC_MJRNETCDF 1

void mjrNC_chkError(int status);
int mjrNC_putAttCStr(int ncid, int varid, char *attName, char *attVal);
void mjrNC_type2str(nc_type ncXType, char *ncXTypeStr);

#endif
