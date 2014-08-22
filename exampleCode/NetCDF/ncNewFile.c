/**
   @file      ncNewFile.c 
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998,2000 by Mitch Richling.  All rights reserved.
   @brief     How to create a NetCDF file.@EOL
   @Keywords  none
   @Std       C89

              This is a simple example illustrating how to create a
              new NetCDF file and populate it with some data.  I have
              been using this bit of code as a template for new NetCDF
              programs for a long time.  It is very similar to the
              example program in the NetCDF documentation; however,
              this code also illustrates real world error checking.

              This example is self contained except for the functions
              starting with "mjrNC_", which can be found in the
              mjrNetCDF.[ch] files.  They are all simple functions
              that serve to take some of the tedium out of using the
              NetCDF library.
*/

#include <netcdf.h>             /* NetCDF Files          */
#include <stdio.h>              /* I/O lib         ISOC  */
#include <string.h>             /* Strings         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */

#include "mjrNetCDF.h"

#define MAX_X 10
#define MAX_Y 20
#define MAX_T 24

#define TST_FILE_NAME "a3DtestFile.nc"

int main(int argc, char *argv[]);

int main(int argc, char *argv[]) {
  int ncFileID, tempID, presID, dimIDs[3];
  int i, j, k, id;
  float temp[MAX_X][MAX_Y][MAX_T];
  float pres[MAX_X][MAX_Y][MAX_T];
  int status;

  /* Create fake data. */
  id=0;
  for(i=0; i<MAX_X; i++)
    for(j=0; j<MAX_Y; j++)
      for(k=0; k<MAX_T; k++) {
        temp[i][j][k] = (1000. + k)/(i+1);
        pres[i][j][k] = id;
        id++;
      } /* end for */

  /* Create the file. */
  status = nc_create(TST_FILE_NAME, NC_CLOBBER, &ncFileID);
  mjrNC_chkError(status);

  /* Note:  The file is in "define mode" after it is created. */

  /* Define three dimensions. */ 
  status = nc_def_dim(ncFileID, "xAxis", MAX_X, &dimIDs[0]);
  mjrNC_chkError(status);
  status = nc_def_dim(ncFileID, "yAxis", MAX_X, &dimIDs[1]);
  mjrNC_chkError(status);
  status = nc_def_dim(ncFileID, "tAxis", MAX_T, &dimIDs[2]);
  mjrNC_chkError(status);

  /* Define two data variables. */
  status = nc_def_var(ncFileID, "temp", NC_FLOAT, 3, dimIDs, &tempID);
  mjrNC_chkError(status);
  status = nc_def_var(ncFileID, "pres", NC_FLOAT, 3, dimIDs, &presID);
  mjrNC_chkError(status);

  /* Define Standard Global Attributes. */ 
  /*  Author of the data file */
  status = mjrNC_putAttCStr(ncFileID, NC_GLOBAL, "author", "Mitch Richling");
  mjrNC_chkError(status);
  /*  Simple title for data set -- used by many generic NetCDF applications. */
  status = mjrNC_putAttCStr(ncFileID, NC_GLOBAL, "title", "Example File");
  mjrNC_chkError(status);
  /*  This attribute is used by many generic NetCDF applications. */
  status = mjrNC_putAttCStr(ncFileID, NC_GLOBAL, "history", "NONE");
  mjrNC_chkError(status);
  /*  Create time for the data file */
  status = mjrNC_putAttCStr(ncFileID, NC_GLOBAL, "ctime", "YYYY-MM-DD HH-MM-SS ZON");
  mjrNC_chkError(status);
  /*  Last modification time */
  status = mjrNC_putAttCStr(ncFileID, NC_GLOBAL, "mtime", "YYYY-MM-DD HH-MM-SS ZON");
  mjrNC_chkError(status);

  /*  Try to use standard unit names and formats (used by many NetCDF programs). */
  status = mjrNC_putAttCStr(ncFileID, tempID, "units", "celcius");
  mjrNC_chkError(status);

  /*  Try to use standard unit names and formats (used by many NetCDF programs). */
  status = mjrNC_putAttCStr(ncFileID, presID, "units", "inHG");
  mjrNC_chkError(status);

  /*  Use this value for labeling plots and the like (used by many NetCDF programs). */
  status = mjrNC_putAttCStr(ncFileID, tempID, "long_name", "Surface Temperature");
  mjrNC_chkError(status);

  /* Take the file out of "define mode" (we are done with metadata stuff) */
  status = nc_enddef(ncFileID);
  mjrNC_chkError(status);

  /* Write the pressure and temperature data. */
  status = nc_put_var_float(ncFileID, tempID, &temp[0][0][0]);
  mjrNC_chkError(status);
  status = nc_put_var_float(ncFileID, presID, &pres[0][0][0]);
  mjrNC_chkError(status);

  /* Close the netCDF file. */
  status = nc_close(ncFileID);
  mjrNC_chkError(status);

  return 0;

} /* end func main */


