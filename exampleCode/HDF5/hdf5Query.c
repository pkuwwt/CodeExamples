/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/* ****************************************************************************************************************************** */
/**
   @file      hdf5Query.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     How to extract various bits of info from an HDF5 file.@EOL
   @Keywords  none
   @Std       C99 HDF5

   While most HDF5 applications must only work with HDF5 files with a relatively well defined format, it is often necessary to
   discover various bits of meta-data at run time.  For example, many applications know the names, data types, ranks, and locations
   of required datasets, but may need to discover dim sizes.
              
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
  hid_t   fileID, dataSetID;
  herr_t  hErrVal;
  int     i;
  hsize_t dims[1024], maxDims[1024];
  H5T_class_t class;
  char classStr[32];
  hid_t dataTypeID;
  size_t dataSize;
  H5T_order_t order;
  int rank; /* Note this is an int, not an hssize_t */
  int intVal;
  hid_t dataSpaceID;
  hid_t rootGroupID;
  hsize_t numInRootGrp, firstDataSetIdx, foundFirstDataSet;
  char attrName[1024], firstDataSetName[1024];
  ssize_t objectNameSize, attrNameSize;
  H5G_stat_t objectStatInfo;
  int numAttrs;
  int curAttrIdx;
  hid_t attrID;
  hsize_t numDataPoints;
  unsigned majnum, minnum, relnum;

  /* Load the library -- not required most platforms. */
  hErrVal = H5open();
  mjrHDF5_chkError(hErrVal);

  /* Get the library version */
  hErrVal = H5get_libversion(&majnum, &minnum, &relnum);
  mjrHDF5_chkError(hErrVal);
  printf("Lib Version: v%lu.%lur%lu\n", (unsigned long)majnum, (unsigned long)minnum, (unsigned long)relnum);

  /*  Open an existing file. */
  fileID = H5Fopen(TST_FILE_NAME, H5F_ACC_RDWR, H5P_DEFAULT);
  mjrHDF5_chkError(fileID);

  /*  Get the ID for the "root" group -- every HDF5 has one */
  rootGroupID = H5Gopen(fileID, "/", H5P_DEFAULT);
  mjrHDF5_chkError(rootGroupID);

  /* Get the number of objects in the root group. */
  hErrVal = H5Gget_num_objs(rootGroupID, &numInRootGrp);
  mjrHDF5_chkError(hErrVal);

  printf("The root group contains %lu object%c\n", (unsigned long)numInRootGrp, (numInRootGrp==1?' ':'s'));

  if(numInRootGrp < 1) {
    printf("As the file contains NO objects, I have nothing left to do...\n");
    exit(1);
  } /* end if */

  /*  Find the first dataset in the root group. */
  for(foundFirstDataSet=0,firstDataSetIdx=0; (!foundFirstDataSet)&&(firstDataSetIdx<numInRootGrp); firstDataSetIdx++) {
    /* Get object name from the index. */
    objectNameSize = H5Gget_objname_by_idx(rootGroupID, firstDataSetIdx, firstDataSetName, 1024);
    mjrHDF5_chkError(objectNameSize);
    if(objectNameSize == 0) { /* Need to check for zero return too */
      printf("ERROR: Object with index %lu doesn't exist in root group!\n", (unsigned long)firstDataSetIdx);
      exit(1);
    } /* end if */
    /*  Now use the object name to get info about the object... */
    hErrVal = H5Gget_objinfo(rootGroupID, firstDataSetName, 0, &objectStatInfo);
    mjrHDF5_chkError(hErrVal);
    /* If the object is a dataset, then print out some info. */
    if(objectStatInfo.type == H5G_DATASET) {
      printf("Object %luth (%s) is a dataset!\n", (unsigned long)firstDataSetIdx, firstDataSetName);
      printf("The name of the %luth object of the root group is: %s\n", (unsigned long)firstDataSetIdx, firstDataSetName);
      foundFirstDataSet = 1;
      printf("Info for the %s dataset:\n", firstDataSetName);
      printf("  Modify time: %lu\n", (unsigned long)objectStatInfo.mtime);
      printf("  Type: %lu\n", (unsigned long)objectStatInfo.type);
      printf("  Link count: %lu\n", (unsigned long)objectStatInfo.nlink);
    } /* end if */
  } /* end for */
  /* Note: At this point index of the dataset will be: firstDataSetIdx-- */
  if(!foundFirstDataSet) {
    printf("ERROR: Could not find a dataset in the root group\n");
    exit(1);
  } /* end if */

  /* Open the dataset we found -- we know it exists. */
  dataSetID = H5Dopen(rootGroupID, firstDataSetName, H5P_DEFAULT);
  mjrHDF5_chkError(dataSetID);

  /* ****************************************************************************************************************************** */
  /* Get some info regarding the TYPE of the dataset. */
  dataTypeID  = H5Dget_type(dataSetID);
  mjrHDF5_chkError(dataTypeID);
  /*  Get the class of the data */
  class = H5Tget_class(dataTypeID);
  mjrHDF5_Tclass2str(class, classStr);
  printf("  Object class: %s\n", classStr);
  /*  Get the size of the type */
  dataSize = H5Tget_size(dataTypeID);
  if(dataSize == 0) {
    printf("ERROR: Failure in H5Tget_size().\n");
    exit(1);
  } /* end if */
  printf("  Size of data type: %lu\n", (unsigned long)dataSize);
  /*  Get the byte order */
  order = H5Tget_order(dataTypeID);
  printf("  Byte Order: ");
  switch(order) {
    case H5T_ORDER_ERROR  : printf("ERROR\n");            break;
    case H5T_ORDER_LE     : printf("Little Endian\n");    break;
    case H5T_ORDER_BE     : printf("Big Endian\n");       break;
    case H5T_ORDER_VAX    : printf("VAX mixed endian\n"); break;
    case H5T_ORDER_MIXED  : printf("Mixed endian\n"); break;
    case H5T_ORDER_NONE   : printf("particular order\n"); break;
  } /* end switch */
  /*  We are done with the datatype. */
  hErrVal = H5Tclose(dataTypeID);
  mjrHDF5_chkError(hErrVal);

  /* ****************************************************************************************************************************** */
  /* Figure out the size of the dataset. */
  dataSpaceID = H5Dget_space(dataSetID);
  mjrHDF5_chkError(dataSpaceID);
  /*  Get the number of dims. */
  rank = H5Sget_simple_extent_ndims(dataSpaceID);
  mjrHDF5_chkError(rank);
  if(rank > 1024) {
    /*  This can't really happen (limit is 32) */
    printf("ERROR: rank too large.\n");
    exit(1);
  } /* end if */
  /* Get the size of each dim. */
  intVal = H5Sget_simple_extent_dims(dataSpaceID, dims, maxDims);
  mjrHDF5_chkError(intVal);
  printf("  Dataspace Rank %lu\n", (unsigned long)rank);
  printf("  Dim Lengths: ");
  for(i=0; i<rank; i++)
    if(dims[i] == H5S_UNLIMITED) {
      printf("%s ", "UNLIMITED");
    } else {
      printf("%ld ", (long)(dims[i]));
    } /* end if/else */
  printf("\n");
  printf("  Max Dim Lengths: ");
  for(i=0; i<rank; i++)
    if(maxDims[i] == H5S_UNLIMITED) {
      printf("%s ", "UNLIMITED");
    } else {
      printf("%ld ", (long)(maxDims[i]));
    } /* end if/else */
  printf("\n");
  numDataPoints = H5Sget_simple_extent_npoints(dataSpaceID);
  if(numDataPoints == 0) {
    printf("ERROR: Call to H5Sget_simple_extent_npoints failed.\n");
    exit(1);
  } /* end if */
  printf("Number of data points: %lu\n", (unsigned long)numDataPoints);

  /* We are done with the dataSpaceID */
  hErrVal = H5Sclose(dataSpaceID);
  mjrHDF5_chkError(hErrVal);

  /* Get the number of attributes for the dataSet. */
  numAttrs = H5Aget_num_attrs(dataSetID);
  mjrHDF5_chkError(numAttrs);

  printf("  Number of attrs: %lu\n", (unsigned long)numAttrs);

  /* If we have any attributes, we get info for them */
  if(numAttrs > 0) {
    printf("  Attribute info:\n");

    for(curAttrIdx=0; curAttrIdx<numAttrs; curAttrIdx++) {
      attrID = H5Aopen_idx(dataSetID, curAttrIdx);
      mjrHDF5_chkError(attrID);

      attrNameSize = H5Aget_name(attrID, 1024, attrName);
      mjrHDF5_chkError(attrNameSize);

      printf("    Number %3lu:  ", (unsigned long)curAttrIdx);

	  dataTypeID  = H5Aget_type(attrID);
	  mjrHDF5_chkError(dataTypeID);
	  /* Get the class for the type. */
	  class = H5Tget_class(dataTypeID);
	  mjrHDF5_Tclass2str(class, classStr);
	  printf(" Class: %-16s", classStr);
	  /*  Get the size of the type */
	  dataSize = H5Tget_size(dataTypeID);
	  if(dataSize == 0) {
		printf("ERROR: Failure in H5Tget_size().\n");
		exit(1);
	  } /* end if */
	  printf(" Size: %3lu ", (unsigned long)dataSize);
	  hErrVal = H5Tclose(dataTypeID);
	  mjrHDF5_chkError(hErrVal);

      printf(" Name: %s \n", attrName);

      hErrVal = H5Aclose(attrID);
	  mjrHDF5_chkError(hErrVal);
    } /* end for */
  } /* end if */

  /* Close the dataset. */
  hErrVal = H5Dclose(dataSetID);

  /* Close the file. */
  hErrVal = H5Fclose(fileID);

  /* Unload the library and free any remaining resources. */
  hErrVal = H5close();
  mjrHDF5_chkError(hErrVal);

  return 0;

} /* end func main */
