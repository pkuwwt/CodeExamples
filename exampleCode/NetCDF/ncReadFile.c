/**
   @file      ncReadFile.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     How to read data from a NetCDF file.@EOL
   @Keywords  NetCDF I/O read
   @Std       C89

              This simple example illustrates a very typical use of
              the NetCDF libraries to read an existing file with known
              variable names.              
*/

#include <netcdf.h>             /* NetCDF Files          */
#include <stdio.h>              /* I/O lib         ISOC  */
#include <string.h>             /* Strings         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */

#include "mjrNetCDF.h"

#define TST_FILE_NAME "a3DtestFile.nc"

int main(int argc, char *argv[]);

int main(int argc, char *argv[]) {
  int status, ncFileID;
  nc_type ncXType, varAttType;
  int numVarAttrs, varAttID;
  size_t varAttLen;
  char varAttValue[8];
  int numVarDims, varDimIDs[3];

  int presID;
  int i, j, k, id;
  size_t dimI, dimJ, dimK;
  float *pres;


  /*  Open the NetCDF file */
  status = nc_open(TST_FILE_NAME, NC_NOWRITE, &ncFileID); 
  mjrNC_chkError(status);

  /*  Get the IDs of variables from known variable names (we only care about 'pres'). */
  status = nc_inq_varid(ncFileID, "pres", &presID);
  mjrNC_chkError(status);

  /*  From the variable IDs, get variable info (we only care about 'pres'). */
  /*  See Also: nc_inq_var, nc_inq_varname, nc_inq_vartype, nc_inq_varndims, nc_inq_vardimid, nc_inq_varnatts */

  /*  We expect pres to be "float", check that.. */
  status =  nc_inq_vartype(ncFileID, presID, &ncXType);
  mjrNC_chkError(status);
  if(ncXType != NC_FLOAT) {
    printf("ERROR: The 'pres' variable was not of type 'float'!\n");
    exit(1);
  } /* end if */

  /*  We also expect it to have units of 'inHG' */
  status =  nc_inq_varndims(ncFileID, presID, &numVarDims);
  mjrNC_chkError(status);
  if(numVarDims != 3) {
    printf("ERROR: The 'pres' variable was not 3D!\n");
    exit(1);
  } /* end if */

  /*  Make sure we have at least ONE attribute.. */
  status = nc_inq_varnatts(ncFileID, presID, &numVarAttrs);
  mjrNC_chkError(status);
  if(numVarAttrs <= 0) {
    printf("ERROR: The 'pres' variable has no attributes.  Need at lesat 'units'!\n");
    exit(1);
  } /* end if */

  /*  Get the ID of the 'units' attribute of the 'pres' variable */
  status = nc_inq_attid(ncFileID, presID, "units", &varAttID);
  /*  The mjrNC_chkError will fail if the attr doesn't exist */
  mjrNC_chkError(status);

  status = nc_inq_atttype(ncFileID, presID, "units", &varAttType);
  mjrNC_chkError(status);
  if(varAttType != NC_CHAR) {
    printf("ERROR: The 'pres' variable has a non-char 'units' attribute!\n");
    exit(1);
  } /* end if */

  status = nc_inq_attlen(ncFileID, presID, "units", &varAttLen);
  mjrNC_chkError(status);
  if(varAttLen != 5) {
    printf("ERROR: The 'pres' variable 'units' are not 'inHG'!\n");
    exit(1);
  } /* end if */

  /*  It is the correct size.  Get it, and make sure it is 'inHG'. */
  status = nc_get_att_text(ncFileID, presID, "units", varAttValue);
  if(strncmp("inHG", varAttValue, 5) != 0) {
    printf("ERROR: The 'pres' variable 'units' are not 'inHG'!\n");
    exit(1);
  } /* end if */

  /*  We do this AFTER we check that the number of dims will fit in our array. */
  status =  nc_inq_vardimid(ncFileID, presID, varDimIDs);
  mjrNC_chkError(status);

  /*  We have three dims, so we call the lengths dimI, dimJ, dimK */
  status = nc_inq_dimlen(ncFileID, varDimIDs[0], &dimI);
  mjrNC_chkError(status);
  status = nc_inq_dimlen(ncFileID, varDimIDs[1], &dimJ);
  mjrNC_chkError(status);
  status = nc_inq_dimlen(ncFileID, varDimIDs[2], &dimK);
  mjrNC_chkError(status);

  /*  Allocate enough space.. */
  pres = (float *)malloc(sizeof(float)*dimI*dimJ*dimK);

  status = nc_get_var_float(ncFileID, presID, pres);
  mjrNC_chkError(status);

  for(id=0,i=0,id=0;i<dimI;i++)
    for(j=0;j<dimJ;j++)
      for(k=0;k<dimK;k++,id++)
          printf("[%02d,%02d,%02d] = %f\n", i, j, k, pres[id]);

  free(pres);

  return 0;
} /* end func main */
