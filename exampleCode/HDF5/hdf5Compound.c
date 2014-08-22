/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/* ****************************************************************************************************************************** */
/**
   @file      hdf5Compound.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     Use of HDF5 compound data types.@EOL
   @Keywords  hdf5 compound types database db
   @Std       C89 HDF5

   This example program demonstrates the details of creating and using HDF5 compound types (think structs) that can be stored into
   an HDF5 array on disk.

   The C structs often have "holes" in them because of hardware word alignment requirements.  This example illustrates a struct that
   has holes on most modern architectures, and how to use an H5T_COMPOUND type to "mirror" the in-RAM layout of this C struct.  In
   addition, this program illustrates how to use H5T_COMPOUND types to represent such a structure on disk in a platform independent,
   space efficient (no holes), manner -- this causes a performance hit because of the required layout conversion upon read/write.

   Compound types may be used to simulate the record/field structure of a database table with HDF5; however, the HDF5 high level
   table API is probably a more direct approach to this particular application.  For such an approach, see hdf5Table.c.
              
*/
/* ------------------------------------------------------------------------------------------------------------------------------ */

/* ****************************************************************************************************************************** */
#include <stdio.h>              /* I/O lib         ISOC  */
#include <string.h>             /* Strings         ISOC  */
#include <hdf5.h>               /* HDF5 files            */

/* ****************************************************************************************************************************** */
#include "mjrHDF5.h"

/* ****************************************************************************************************************************** */
/* This struct can be "tuned" to have "holes" on just about any hardware platform.  It is thus a good example of how to create HDF5
   compound data types that describe things in memory vs. describing them on disk in a portable way.*/
#define PR_NAME_LEN 32
typedef struct personRecordStr {
  char           name[PR_NAME_LEN];
  unsigned char  age;
  int            weight;
  float          IQ;
} personRecord_t;

/* ****************************************************************************************************************************** */
#define NUM_RECS 10000
#define TST_FILE_NAME "aCpndtestFile.h5"

/* ****************************************************************************************************************************** */
int main(int argc, char *argv[]) {
  personRecord_t aPersonArr[NUM_RECS];
  hid_t memoryPersonRecordTypeID, diskPersonRecordTypeID;
  hid_t stringTypeID;
  int offset;
  hsize_t numRecs = NUM_RECS;
  int i;
  hid_t   fileID, datasetID, dataspaceID;
  herr_t  hErrVal;
  hsize_t chunkDims = NUM_RECS;
  hid_t dataSetPList;
  int sizeOfParts, sizeOfWhole;

  /* We can test for holes by comparing the struct size to the size of the components. */
  sizeOfParts = (int)(sizeof(char)*33+sizeof(int)+sizeof(float));
  sizeOfWhole = (int)(sizeof(personRecord_t));
  printf("sizeof(personRecord_t)=%d\n", sizeOfWhole);
  printf("sizeof(char[PR_NAME_LEN]+char+int+float)=%d\n", sizeOfParts);
  if(sizeOfWhole > sizeOfParts) 
    printf("A personRecord_t HAS holes!\n");
  else
    printf("A personRecord_t has NO holes!\n");

  /* Fill in some bogus data for the the array. */
  for(i=0;i<NUM_RECS;i++) {
    strcpy(aPersonArr[i].name, "Mitch Richling");
    aPersonArr[i].age    = i+23;
    aPersonArr[i].weight = 123+i;
    aPersonArr[i].IQ     = 200-(i*2.5+2)/(i+1);
  }
  
  /* Load the library -- not required on most platforms. */
  hErrVal = H5open();
  mjrHDF5_chkError(hErrVal);

  /* Create the string type for the .name part of a personRecord_t */
  stringTypeID = H5Tcopy(H5T_C_S1);
  mjrHDF5_chkError(stringTypeID);
  hErrVal = H5Tset_size(stringTypeID, PR_NAME_LEN);
  mjrHDF5_chkError(hErrVal);

  /* Create the type for an in-RAM representation of a personRecord_t */
  memoryPersonRecordTypeID = H5Tcreate(H5T_COMPOUND, sizeof(personRecord_t));
  mjrHDF5_chkError(memoryPersonRecordTypeID);
  hErrVal = H5Tinsert(memoryPersonRecordTypeID, "name",   HOFFSET(personRecord_t, name),   stringTypeID);
  mjrHDF5_chkError(hErrVal);
  hErrVal = H5Tinsert(memoryPersonRecordTypeID, "age",    HOFFSET(personRecord_t, age),    H5T_NATIVE_CHAR);
  mjrHDF5_chkError(hErrVal);
  hErrVal = H5Tinsert(memoryPersonRecordTypeID, "weight", HOFFSET(personRecord_t, weight), H5T_NATIVE_INT);
  mjrHDF5_chkError(hErrVal);
  hErrVal = H5Tinsert(memoryPersonRecordTypeID, "IQ",     HOFFSET(personRecord_t, IQ),     H5T_NATIVE_FLOAT);
  mjrHDF5_chkError(hErrVal);

  /* Create the type for an on-disk representation of a personRecord_t */
  diskPersonRecordTypeID = H5Tcreate(H5T_COMPOUND, sizeof(personRecord_t));
  mjrHDF5_chkError(diskPersonRecordTypeID);
  offset=0;
  hErrVal = H5Tinsert(diskPersonRecordTypeID, "name",   offset, stringTypeID);
  mjrHDF5_chkError(hErrVal);
  offset += sizeof(char)*PR_NAME_LEN;
  hErrVal = H5Tinsert(diskPersonRecordTypeID, "age",    offset, H5T_STD_U8LE);
  mjrHDF5_chkError(hErrVal);
  offset += sizeof(unsigned char);
  hErrVal = H5Tinsert(diskPersonRecordTypeID, "weight", offset, H5T_STD_I32LE);
  mjrHDF5_chkError(hErrVal);
  offset += sizeof(int);
  hErrVal = H5Tinsert(diskPersonRecordTypeID, "IQ",     offset, H5T_IEEE_F32LE);
  mjrHDF5_chkError(hErrVal);

  /* Create the data space for the dataset. */
  dataspaceID = H5Screate_simple(1, &numRecs, NULL);
  mjrHDF5_chkError(dataspaceID);

  /* Create a new file using default properties. */
  fileID = H5Fcreate(TST_FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  mjrHDF5_chkError(fileID);

  /* Create a default dataset plist. */
  dataSetPList = H5Pcreate(H5P_DATASET_CREATE);
  mjrHDF5_chkError(dataSetPList);
  /* Set a chunk size (required for extension) */
  hErrVal = H5Pset_chunk(dataSetPList, 1, &chunkDims);
  mjrHDF5_chkError(hErrVal);
  /* Set compression (GZIP) with compression effort set to 9.*/
  hErrVal = H5Pset_deflate(dataSetPList, 9);
  mjrHDF5_chkError(hErrVal);
  /* Set Shuffling for better compression. */
  hErrVal = H5Pset_shuffle(dataSetPList);
  mjrHDF5_chkError(hErrVal);

  /* Create the dataset. */
  datasetID = H5Dcreate(fileID, "/dset", diskPersonRecordTypeID, dataspaceID, H5P_DEFAULT, dataSetPList, H5P_DEFAULT);
  mjrHDF5_chkError(datasetID);

  /* Write some data into our dataset. */
  hErrVal = H5Dwrite(datasetID, memoryPersonRecordTypeID, H5S_ALL, H5S_ALL, H5P_DEFAULT, aPersonArr);
  mjrHDF5_chkError(hErrVal);

  /* End access to the dataset and release resources used by it. */
  hErrVal = H5Dclose(datasetID);
  mjrHDF5_chkError(hErrVal);

  /* Terminate access to the data space. */ 
  hErrVal = H5Sclose(dataspaceID);
  mjrHDF5_chkError(hErrVal);

  /* Terminate access to the data types. */ 
  hErrVal = H5Tclose(stringTypeID);
  mjrHDF5_chkError(hErrVal);
  hErrVal = H5Tclose(memoryPersonRecordTypeID);
  mjrHDF5_chkError(hErrVal);
  hErrVal = H5Tclose(diskPersonRecordTypeID);
  mjrHDF5_chkError(hErrVal);
  
  /* Close the file. */
  hErrVal = H5Fclose(fileID);
  mjrHDF5_chkError(hErrVal);

  /* Unload the library and free any remaining resources. */
  hErrVal = H5close();
  mjrHDF5_chkError(hErrVal);

  return 0;
} /* end func main */
