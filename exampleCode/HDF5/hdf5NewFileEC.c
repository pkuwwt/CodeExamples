/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/* ****************************************************************************************************************************** */
/**
   @file      hdf5NewFileEC.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     How to create an extensible dataset..@EOL
   @Keywords  none
   @Std       C89

   The intent of this somewhat incomplete example is to illustrate the differences to hdf5NewFile.c required in order to make the
   dataset extensible and/or to enable compression.  I have highlighted the differences with comments containing (EXT_DIFF).
              
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
  hid_t fileID, datasetID, dataspaceID;
  hsize_t dims[3]      = {MAX_X,   MAX_Y,   MAX_T};
  hsize_t maxDims[3]   = {MAX_X*2, MAX_Y*2, H5S_UNLIMITED}; /* (EXT_DIFF) */
  /* For maximum compression set the chunk size to the size of the data set.  For maximum storage efficiency in the face of dataset
     growth, pick a chunk size that is a unit of typical growth.  It can be difficult, or impossible, to pick a chunk size that
     supports both growth (extension) and compression in an optimal way.  It can also be difficult to pick a good size for highly
     variable growth rates.  A very common choice is a chunk size that is the expected initial size of the dataset (as is done
     here). */
  hsize_t chunkDims[3] = {MAX_X,   MAX_Y,   MAX_T};         /* (EXT_DIFF) */
  herr_t  hErrVal;
  hid_t dataSetPList;

  /* Load the library -- not required for most platforms. */
  hErrVal = H5open();
  mjrHDF5_chkError(hErrVal);

  /* Create a new file using default properties. */
  fileID = H5Fcreate(TST_FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  mjrHDF5_chkError(fileID);

  /* Annotate root group here (global attributes) */

  /* Create the data space for the dataset. */
  dataspaceID = H5Screate_simple(3, dims, maxDims); /* (EXT_DIFF) */
  mjrHDF5_chkError(dataspaceID);

  /* Create a default dataset plist. */
  dataSetPList = H5Pcreate(H5P_DATASET_CREATE); /* (EXT_DIFF) */
  mjrHDF5_chkError(dataSetPList);
  /* Set a chunk size (required for extension) */
  hErrVal = H5Pset_chunk(dataSetPList, 3, chunkDims); /* (EXT_DIFF) */
  mjrHDF5_chkError(hErrVal);
  /* Set compression (GZIP) with compression effort set to 9.  Chunking is required for compression, but one need not compress all
     chunked data sets.*/
  hErrVal = H5Pset_deflate(dataSetPList, 9); /* (EXT_DIFF) */
  mjrHDF5_chkError(hErrVal);
  /* Shuffling can lead to better compression. */
  hErrVal = H5Pset_shuffle(dataSetPList); /* (EXT_DIFF) */
  mjrHDF5_chkError(hErrVal);

  /* Create the dataset. */
  datasetID = H5Dcreate(fileID, "/dset", H5T_IEEE_F32BE, dataspaceID, dataSetPList, H5P_DEFAULT, H5P_DEFAULT); /* (EXT_DIFF) */
  mjrHDF5_chkError(datasetID);

  /* Annotate the dataset & dims */
  /* Write some data into our dataset. */

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

