/**
   @file      ncHeaderDump.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1999 by Mitch Richling.  All rights reserved.
   @brief     How to discover what is in a NetCDF file@EOL
   @Keywords  NetCDF
   @Std       C89

              This example program demonstrates how to use the various
              NetCDF query functions to discover what is in a NetCDF
              file.  Functionally speaking, this program is very
              similar to the operation of `ncdump -h`, except that
              this program doesn't extract the values of attributes or
              print in the canonical ASCII CDF format.  For a more
              comprehensive example, see the ncdump source code that
              comes with the NetCDF distribution.
*/

#include <netcdf.h>             /* NetCDF Files          */
#include <stdio.h>              /* I/O lib         ISOC  */
#include <string.h>             /* Strings         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */

#include "mjrNetCDF.h"

#define TST_FILE_NAME "a3DtestFile.nc"

int main(int argc, char *argv[]);

int main(int argc, char *argv[]) {
  int status, ncID, numDims, numVars, numGblAttrs, unlimDimID;
  size_t dimLength, attrContentLength;
  char nameBuf[NC_MAX_NAME+1];
  nc_type ncXType;
  int numVarAttrs, numVarDims;
  int varDimIDs[NC_MAX_VAR_DIMS];
  int varID, attNum, dimID;
  char ncXTypeStr[16];

  /*  Open the NetCDF file */
  status = nc_open(TST_FILE_NAME, NC_NOWRITE, &ncID); 
  mjrNC_chkError(status);

  /*  Collect object counts within the file */
  status = nc_inq(ncID, &numDims, &numVars, &numGblAttrs, &unlimDimID); 
  /*  See Also: nc_inq_ndims, nc_inq_nvars, nc_inq_natts, nc_inq_unlimdim */
  mjrNC_chkError(status);

  printf("General Info:\n");
  printf("  #dimensions(%03d) #variables(%03d) #globalAttributes(%03d)\n",
         numDims, numVars, numGblAttrs);
  if(unlimDimID<0) 
    printf("  No dimensions are unlimited\n");
  else
    printf("  One dimension is unlimited (%d)\n", unlimDimID);

  printf("Dimensions:\n");
  for(dimID=0; dimID<numDims; dimID++) {
    status = nc_inq_dim(ncID, dimID, nameBuf, &dimLength);
    /*  See Also: nc_inq_dimlen, nc_inq_dimname */
    mjrNC_chkError(status);
    printf("  dim(%03d): Length: %3d Name: %s\n", dimID, (int)dimLength, nameBuf);
  } /* end for */

  printf("Variables:\n");
  for(varID=0; varID<numVars; varID++) {
    status =  nc_inq_var(ncID, varID, nameBuf, &ncXType, &numVarDims, varDimIDs, &numVarAttrs);
    /*  See Also: nc_inq_varname, nc_inq_vartype, nc_inq_varndims, nc_inq_vardimid, nc_inq_varnatts */
    mjrNC_chkError(status);
    mjrNC_type2str(ncXType, ncXTypeStr);
    printf("  var(%03d) type(%s) #dim(%03d) dimIDs(", varID, ncXTypeStr, numVarDims);
    for(dimID=0; dimID<numVarDims; dimID++)
      printf("%03d ", varDimIDs[dimID]);
    printf(") #attrs: %03d name: %s\n", numVarAttrs, nameBuf);
    if(numVarAttrs>0) {
      printf("    Variable Attributes:\n");
      for(attNum=0; attNum<numVarAttrs; attNum++) {
        status = nc_inq_attname(ncID, varID, attNum, nameBuf);
        /*  See Also (to get the id from the name): nc_inq_attid */
        mjrNC_chkError(status);
        nc_inq_att(ncID, varID, nameBuf, &ncXType, &attrContentLength);
        /*  See Also: nc_inq_atttype, nc_inq_attlen */
        mjrNC_chkError(status);
        mjrNC_type2str(ncXType, ncXTypeStr);
        printf("    attr(%03d) type: %s size: %3d name: %s\n",
               attNum, ncXTypeStr, (int)attrContentLength, nameBuf);
      } /* end for */
    } /* end if */
  } /* end for */

  printf("Global Attributes:\n");
  for(attNum=0; attNum<numGblAttrs; attNum++) {
    status = nc_inq_attname(ncID, NC_GLOBAL, attNum, nameBuf);
    /*  See Also (to get the id from the name): nc_inq_attid */
    mjrNC_chkError(status);
    status = nc_inq_att(ncID, NC_GLOBAL, nameBuf, &ncXType, &attrContentLength);
    /*  See Also: nc_inq_atttype, nc_inq_attlen */
    mjrNC_chkError(status);
    mjrNC_type2str(ncXType, ncXTypeStr);
    printf("  attr(%03d) type(%s) size(%3d) name(%s)\n",
           attNum, ncXTypeStr, (int)attrContentLength, nameBuf);
  } /* end for */

  return 0;
} /* end func main */
