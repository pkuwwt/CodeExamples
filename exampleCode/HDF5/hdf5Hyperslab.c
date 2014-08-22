/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/* ****************************************************************************************************************************** */
/**
   @file      hdf5Hyperslab.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     How to select, read, and write HDF5 hyperslabs.@EOL
   @Keywords  HDF5 hyperslab
   @Std       C89

   How to read a subset of an HDF5 dataset into a subset of an in-RAM array -- via the use of hyperslabs.  The ability to select
   hyperslabs within a dataset is one of HDF5's most useful features.
              
*/
/* ------------------------------------------------------------------------------------------------------------------------------ */

/* ****************************************************************************************************************************** */
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
  herr_t  hErrVal;
  int     i, j;
  float   ramData[MAX_X+2][MAX_Y+2];
  hsize_t fileSlabCount[3] = {MAX_X, MAX_Y, 1};    /* Get whole slice, one unit thick. */
  hsize_t fileSlabOffset[3] = {0, 0, 3};           /* Get 3rd time stamp.              */
  hsize_t ramSlabCount[2]  = {MAX_X, MAX_Y};       /* Get whole slice                  */
  hsize_t ramSlabOffset[2] = {1, 1};               /* Offset 1 (zero is origin)        */
  hsize_t ramDims[2] = {MAX_X+2, MAX_Y+2};
  hid_t   ramDataspaceID;

  /* Initialize our in-RAM array to -1's. */
  for(i=0;i<(MAX_X+2);i++)
    for(j=0;j<(MAX_Y+2);j++)
      ramData[i][j] = -1.0;

  /* Load the library -- not required on most platforms. */
  hErrVal = H5open();
  mjrHDF5_chkError(hErrVal);

  /* Open an existing file. */
  fileID = H5Fopen(TST_FILE_NAME, H5F_ACC_RDWR, H5P_DEFAULT);
  mjrHDF5_chkError(fileID);

  /* Open an existing dataset. */
  datasetID = H5Dopen(fileID, "/dset", H5P_DEFAULT);
  mjrHDF5_chkError(datasetID);

  /* Get the dataspace for the dataset -- we select hyperslabs from the dataset via the dataspace. */
  dataspaceID = H5Dget_space(datasetID);
  mjrHDF5_chkError(dataspaceID);

  /* Define a hyperslab in the dataset. */
  hErrVal = H5Sselect_hyperslab(dataspaceID, H5S_SELECT_SET, fileSlabOffset, NULL, fileSlabCount, NULL);
  mjrHDF5_chkError(hErrVal);

  /* Create a dataspace for our array, and select a hyperslab within it. */
  ramDataspaceID = H5Screate_simple(2, ramDims, NULL);
  mjrHDF5_chkError(ramDataspaceID);
  hErrVal = H5Sselect_hyperslab(ramDataspaceID, H5S_SELECT_SET, ramSlabOffset, NULL, ramSlabCount, NULL);
  mjrHDF5_chkError(hErrVal);

  /* Read our selection from the file, and place it into the array. */
  hErrVal = H5Dread(datasetID, H5T_NATIVE_FLOAT, ramDataspaceID, dataspaceID, H5P_DEFAULT, ramData);

  /* Print out our in-RAM array. */
  printf("Our in-RAM array now: \n");
  for(i=0;i<(MAX_X+2);i++) {
    for(j=0;j<(MAX_Y+2);j++)
      printf("%7.2f ", ramData[i][j]);
    printf("\n");
  } /* end for */

  /* Close the dataspace */
  hErrVal = H5Sclose(ramDataspaceID);
  mjrHDF5_chkError(hErrVal);

  /* Close the dataset. */
  hErrVal = H5Dclose(datasetID);
  mjrHDF5_chkError(hErrVal);

  /* Close the file. */
  hErrVal = H5Fclose(fileID);
  mjrHDF5_chkError(hErrVal);

  /* Unload the library and free any remaining resources. */
  hErrVal = H5close();
  mjrHDF5_chkError(hErrVal);

  return 0;
} /* end func main */
