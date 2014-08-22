/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/* ****************************************************************************************************************************** */
/**
   @file      hdf5NewFile.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     Create and annotate one 3D data set in an HDF5 file.@EOL
   @Keywords  none
   @Std       C89

   This program serves as an example (and template) for how to create a well documented HDF5 file from scratch.

   The design of the HDF5 API requires a bit of code repetitiveness (the C++ version is not as bad). This is particularly true for
   simple things like adding an attribute.  To help with this, much of the "meat" for the attribute code is contained in functions
   (see mjrHDF5.[hc]).  The error checking for this program is quite simplistic, and doesn't even scratch the surface of the error
   checking that HDF5 is capable of.  Aside from the attribute and error checking functions, this source code is self contained.
   HDF5 is interesting in that a dataspace & dataset can be created independently from the existing of any data -- for completeness,
   this example also fills the data file with some random data.
   
   In a nutshell, this program creates and annotates one 3D data set.

     Data:

       - A 3D data set: temperature at grid (lat, lon, time) points

     Annotations:

       - File: author, title
       - Data: name, units, pressure at which measurement was made
       - Dims: names, units, start, and step values.
              
*/
/* ------------------------------------------------------------------------------------------------------------------------------ */

/* ****************************************************************************************************************************** */
#include <stdio.h>              /* I/O lib         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */
#include <string.h>             /* Strings         ISOC  */
#include <hdf5.h>               /* HDF5 files            */

/* ****************************************************************************************************************************** */
#include "mjrHDF5.h"

/* ****************************************************************************************************************************** */
#define MAX_X 10
#define MAX_Y 20
#define MAX_T 24

#define TST_FILE_NAME "a3DtestFile.h5"

/* ****************************************************************************************************************************** */
int main(int argc, char *argv[]) {
  hid_t   fileID, datasetID, dataspaceID;
  hsize_t dims[3] = {MAX_X, MAX_Y, MAX_T};
  herr_t  hErrVal;
  float   floatDataPoint[10];
  int     id, i, j, k;
  float   temp[MAX_X][MAX_Y][MAX_T];
  char    *strPerDim[3];
  float   valPerDim[3];

  /* Create phony data. */
  for(i=0,id=0; i<MAX_X; i++)
    for(j=0; j<MAX_Y; j++)
      for(k=0; k<MAX_T; k++)
        temp[i][j][k] = id++;
   
  /* Load the library -- not required on most platforms. */
  hErrVal = H5open();
  mjrHDF5_chkError(hErrVal);

  /* Create a new file using default properties. */
  fileID = H5Fcreate(TST_FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  mjrHDF5_chkError(fileID);

  hErrVal = mjrHDF5_put_gblAtt_oneFCstr(fileID, "Author", "Mitch Richling");
  mjrHDF5_chkError(hErrVal);
  hErrVal = mjrHDF5_put_gblAtt_oneFCstr(fileID, "title",  "Example File");
  mjrHDF5_chkError(hErrVal);

  /* Create the data space for the dataset. */
  dataspaceID = H5Screate_simple(3, dims, NULL);
  mjrHDF5_chkError(dataspaceID);

  /* Create the dataset. */
  datasetID = H5Dcreate(fileID, "/dset", H5T_IEEE_F32BE, dataspaceID, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  mjrHDF5_chkError(datasetID);

  /*  Try to use standard unit names and formats... */
  hErrVal = mjrHDF5_put_att_oneFCstr(datasetID, "units", "Celsius");
  mjrHDF5_chkError(hErrVal);

  /* Use this value for labeling plots and the like by programs that
     also support netCDF */
  hErrVal = mjrHDF5_put_att_oneFCstr(datasetID, "long_name", "Some Name For Plots");
  mjrHDF5_chkError(hErrVal);

  /* Add an array of strings to name each dim.*/
  strPerDim[0] = "lat";
  strPerDim[1] = "lon";
  strPerDim[2] = "temp";
  hErrVal = mjrHDF5_put_att_arrVCstr(datasetID, "dimNames", strPerDim, 3);
  mjrHDF5_chkError(hErrVal);

  /* Add an array of strings to to give units for each dim.*/
  strPerDim[0] = "degrees";
  strPerDim[1] = "degrees";
  strPerDim[2] = "hours";
  hErrVal = mjrHDF5_put_att_arrVCstr(datasetID, "dimUnits", strPerDim, 3);
  mjrHDF5_chkError(hErrVal);

  /* Add annotation describing minimum values for each dim. */
  valPerDim[0] = 0.0;
  valPerDim[1] = 0.0;
  valPerDim[2] = 0.0;
  hErrVal = mjrHDF5_put_att_arry(datasetID, H5T_IEEE_F32BE, "dimStart", valPerDim, H5T_NATIVE_FLOAT, 3);
  mjrHDF5_chkError(hErrVal);

  /* Add annotation describing the "step" between values on each dim. */
  valPerDim[0] = 10.0;
  valPerDim[1] = 20.0;
  valPerDim[2] = 24.0;
  hErrVal = mjrHDF5_put_att_arry(datasetID, H5T_IEEE_F32BE, "dimStep", valPerDim, H5T_NATIVE_FLOAT, 3);
  mjrHDF5_chkError(hErrVal);

  /* Just for fun, create a floating point attribute! */
  floatDataPoint[0] = 1.234;
  mjrHDF5_put_att_sclr(datasetID, H5T_IEEE_F32BE, "Pressure", floatDataPoint, H5T_NATIVE_FLOAT);

  /* Write some data into our dataset. */
  hErrVal = H5Dwrite(datasetID, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, temp);
  mjrHDF5_chkError(hErrVal);

  /* End access to the dataset and release resources used by it. */
  hErrVal = H5Dclose(datasetID);
  mjrHDF5_chkError(hErrVal);

  /* Terminate access to the data space. */ 
  hErrVal = H5Sclose(dataspaceID);
  mjrHDF5_chkError(hErrVal);
  
  /* Close the file. */
  hErrVal = H5Fclose(fileID);
  mjrHDF5_chkError(hErrVal);

  /* Unload the library and free any remaining resources. */
  hErrVal = H5close();
  mjrHDF5_chkError(hErrVal);

  return 0;
} /* end func main */

