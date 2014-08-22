/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/* ****************************************************************************************************************************** */
/**
   @file      hdf5ReadModifyWrite.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1997 by Mitch Richling.  All rights reserved.
   @brief     Read, modify, and write a dataset in an existing HDF5 file. @EOL
   @Keywords  hdf5
   @Std       C89

   This simple example shows how most applications use an existing HDF5 file.  It reads a file with a known format, modifies the
   data in RAM, and then writes the new data back to the file.

   Most of the time, applications are working with a file with a known structure (the data types, dims, locations, and names of
   datasets are known before hand).  For this use case, it is very easy to make use of HDF5.
               
   While this simplicity is very tempting, it is a very good way to create applications that are easily broken by very minor changes
   in the source of your HDF5 data.

   Personal note: I personally feel that it is a crime, perhaps even a sin, to create HDF5 files that are not completely self
   documenting. It will be quite nice to be able to tell what is in that oddly named data file you find 10 years from now on some
   forgotten disk drive.
              
*/
/* ------------------------------------------------------------------------------------------------------------------------------ */

/* ****************************************************************************************************************************** */
#include <stdio.h>              /* I/O lib         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */
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
  hid_t   fileID, datasetID;
  herr_t  hErrVal;
  int     i, j, k;
  float   temp[MAX_X][MAX_Y][MAX_T];

  /* Load the library -- not required for most platforms. */
  hErrVal = H5open();
  mjrHDF5_chkError(hErrVal);

  /*  Open an existing file. */
  fileID = H5Fopen(TST_FILE_NAME, H5F_ACC_RDWR, H5P_DEFAULT);
  mjrHDF5_chkError(fileID);

  /*  Open an existing dataset. */
  datasetID = H5Dopen(fileID, "/dset", H5P_DEFAULT);
  mjrHDF5_chkError(datasetID);

  /*  Read the dataset from disk */
  hErrVal = H5Dread(datasetID, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, temp);
  mjrHDF5_chkError(hErrVal);

  /*  Modify the Data. */
  for(i=0; i<MAX_X; i++)
    for(j=0; j<MAX_Y; j++)
      for(k=0; k<MAX_T; k++)
        temp[i][j][k] = (temp[i][j][k] + 1) * 2.5;

  /*  Write the modified dataset back to disk. */
  hErrVal = H5Dwrite(datasetID, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, temp);
  mjrHDF5_chkError(hErrVal);

  /*  Close the dataset. */
  hErrVal = H5Dclose(datasetID);
  mjrHDF5_chkError(hErrVal);

  /*  Close the file. */
  hErrVal = H5Fclose(fileID);
  mjrHDF5_chkError(hErrVal);

  /* Unload the library and free any remaining resources. */
  hErrVal = H5close();
  mjrHDF5_chkError(hErrVal);

  return 0;
} /* end func main */
