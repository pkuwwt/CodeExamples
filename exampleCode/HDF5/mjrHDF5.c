/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/* ****************************************************************************************************************************** */
/**
   @file      mjrHDF5.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1997 by Mitch Richling.  All rights reserved.
   @brief     hello   @EOL
   @Keywords  none
   @Std       C89

   Some simple helper functions to make life with HDF5 a little bit less tedious.
              
*/
/* ------------------------------------------------------------------------------------------------------------------------------ */

/* ****************************************************************************************************************************** */
#include <hdf5.h>               /* HDF5 files            */
#include "mjrHDF5.h"

/* ****************************************************************************************************************************** */
#include <stdio.h>              /* I/O lib         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */
#include <string.h>             /* Strings         ISOC  */


/* ****************************************************************************************************************************** */
// /* A note about the HDF5 Lite API... */
//
// /* Need to include this one for all the high level APIs */
// #include <hdf5_hl.h>
// 
// char    strDatO[32] = "Hello, Ma!";
// int     intDatO[32] = {1,2,3,4,5};
// char    strDatI[32];
// int     intDatI[32];
// 
// /* Write and read a string attribute -- note we need to know the size of the attribute for the read */
// hErrVal = H5LTset_attribute_string(fileId, "dataSetName", "attrName", strDatO);
// hErrVal = H5LTget_attribute_string(fileId, "dataSetName", "attrName", strDatI);
// printf("String: %s\n", strDatI);
// 
// /* Write and read an int array attribute -- note we need to know the size of the attribute for the read */
// hErrVal = H5LTset_attribute_int(fileId, "dataSetName", "attrName", intDatO, 5);
// hErrVal = H5LTget_attribute_int(fileId, "dataSetName", "attrName", intDatI);
// printf("Ints:   ");
// int i;
// for(i=0; i<5; i++ )
//   printf("%d ", intDatI[i]);
// printf("\n");
/* ****************************************************************************************************************************** */


/* ****************************************************************************************************************************** */
/* This is a wrapper for the mjrHDF5_put_att_oneFCstr() function to aid in the creation of "global" string valued attributes.

   Note HDF5 files have a hierarchical structure with subdirectory-like "groups" with leaf-node groups corresponding roughly to an
   entire NetCDF file.  Because of this organizational sophistication, a NetCDF-like "global" attribute generally should correspond
   to an HDF attribute attached to the "group" containing the data. For simple applications with one group, this is the "/" (root)
   group.

   Even for complex applications, convention suggests that globally oriented attributes should simply be attached to the root group
   object -- i.e attributes related to an entire "tree" of groups, should be attached to the root node of the subtree. */
herr_t mjrHDF5_put_gblAtt_oneFCstr(hid_t fileID, char *name, char *value) {
  hid_t rootGroupID;

  /* Get the ID for the "root" group -- every HDF5 has one (it is created as part of the H5Fcreate() function). */
  rootGroupID = H5Gopen(fileID, "/", H5P_DEFAULT);
  if(rootGroupID < 0)
    return rootGroupID;

  return mjrHDF5_put_att_oneFCstr(rootGroupID, name, value);
} /* end func mjrHDF5_put_att_oneFCstr */


/* ****************************************************************************************************************************** */
/* This is a little function to encapsulate the steps required to add a correctly sized string attribute to an object (Create a
   correctly sized string type, and call mjrHDF5_put_att_sclr() to create the attribute). */
herr_t mjrHDF5_put_att_oneFCstr(hid_t objectID, char *name, char *value) {
  herr_t status;
  hid_t  str_type;

  /* Create a data type, and set its length. */
  str_type = H5Tcopy(H5T_C_S1);
  if(str_type < 0)
    return str_type;

  status = H5Tset_size(str_type, strlen(value)+2);
  if(status < 0) 
    return status;

  return mjrHDF5_put_att_sclr(objectID, str_type, name, value, str_type);

} /* end func mjrHDF5_put_att_oneFCstr */

/* ****************************************************************************************************************************** */
/* This is a little function to encapsulate the steps required to add a scalar attribute to an object. */
herr_t mjrHDF5_put_att_sclr(hid_t objectID, hid_t attr_type, char *name, void *value, hid_t value_type) {
  hid_t attr_dataspaceID, attributeID;
  herr_t status;

  /* Create the data space for the attribute. */
  attr_dataspaceID = H5Screate(H5S_SCALAR);
  if(attr_dataspaceID < 0) 
    return attr_dataspaceID;
   
  /* Create a dataset attribute. */
  attributeID = H5Acreate(objectID,
                          name,
                          attr_type,
                          attr_dataspaceID,
                          H5P_DEFAULT,
                          H5P_DEFAULT);
  if(attributeID < 0) 
    return attributeID;

  /* Write the attribute data. */
  status = H5Awrite(attributeID, 
                    value_type,
                    value);
  if(status < 0) 
    return status;
   
  /* Close the attribute. */
  status = H5Aclose(attributeID);
  if(status < 0) 
    return status;
   
  /* Close the dataspace. */
  status = H5Sclose(attr_dataspaceID);
  if(status < 0) 
    return status;

  return 0;
} /* end func mjrHDF5_put_att_sclr */

/* ****************************************************************************************************************************** */
/* Most HDF5 functions return a negative number upon error -- even functions that return IDs.  This uniformity makes it easy to
   check for errors.  In this example we simply exit -- note HDF5 has sophisticated error handling facilities. */
void mjrHDF5_chkError(int status) {
  if (status < 0) {
    fprintf(stderr, "HDF5 ERROR: \n");
    H5Eprint(H5E_DEFAULT, stderr);
    exit(1); 
  } /* end if */
} /* end func mjrHDF5_chkError */


/* ****************************************************************************************************************************** */
/* This is a little function to encapsulate the steps required to add a 1D array attribute containing variable length data elements
   to an object. The most common use for this is to create arrays of variable length strings via the mjrHDF5_put_att_arrVCstr()
   function.*/
herr_t mjrHDF5_put_att_arry(hid_t objectID, hid_t attr_type, char *name, void *values, hid_t value_type, hsize_t rank) {
  hid_t attr_dataspaceID, attributeID;
  herr_t status;

  /* Create the data space for the attribute. */
  attr_dataspaceID = H5Screate_simple(1, &rank, NULL);
  if(attr_dataspaceID < 0) 
    return attr_dataspaceID;

  /* Create a dataset attribute. */  
  attributeID = H5Acreate(objectID,
                          name,
                          attr_type,
                          attr_dataspaceID, 
                          H5P_DEFAULT,
                          H5P_DEFAULT);
  if(attributeID < 0) 
    return attributeID;

  /* Write the attribute data. */
  status = H5Awrite(attributeID, 
                    value_type,
                    values);
  if(status < 0) 
    return status;
   
  /* Close the attribute. */
  status = H5Aclose(attributeID);
  if(status < 0) 
    return status;
   
  /* Close the dataspace. */
  status = H5Sclose(attr_dataspaceID);
  if(status < 0) 
    return status;

  return 0;
}  /* end func mjrHDF5_put_att_arry */


/* ****************************************************************************************************************************** */
/* This is a little function to encapsulate the steps required to add an attribute containing a list of variable length strings. */
herr_t mjrHDF5_put_att_arrVCstr(hid_t objectID, char *name, char **values,  hsize_t rank) {
  hid_t attr_type;
  herr_t  status;

  attr_type = H5Tcopy(H5T_C_S1);
  if(attr_type < 0) 
    return attr_type;

  status = H5Tset_size(attr_type, H5T_VARIABLE);
  if(status < 0) 
    return status;

  return mjrHDF5_put_att_arry(objectID, attr_type, name, values, attr_type, rank);
} /* end func mjrHDF5_put_att_arrVCstr */


/* Convert an H5T_Tclass_t to a string.  Pointer must point to a space
   at least 16 chars long. */
void mjrHDF5_Tclass2str(H5T_class_t class, char *classStr) {
  switch(class) {
    case H5T_NO_CLASS   : strcpy(classStr, "NO_CLASS  "); break;
    case H5T_INTEGER    : strcpy(classStr, "INTEGER   "); break;
    case H5T_FLOAT      : strcpy(classStr, "FLOAT     "); break;
    case H5T_TIME       : strcpy(classStr, "TIME      "); break;
    case H5T_STRING     : strcpy(classStr, "STRING    "); break;
    case H5T_BITFIELD   : strcpy(classStr, "BITFIELD  "); break;
    case H5T_OPAQUE     : strcpy(classStr, "OPAQUE    "); break;
    case H5T_COMPOUND   : strcpy(classStr, "COMPOUND  "); break;
    case H5T_REFERENCE  : strcpy(classStr, "REFERENCE "); break;
    case H5T_ENUM       : strcpy(classStr, "ENUM      "); break;
    case H5T_VLEN       : strcpy(classStr, "VLEN      "); break;
    case H5T_ARRAY      : strcpy(classStr, "ARRAY     "); break;
    case H5T_NCLASSES   : strcpy(classStr, "NCLASSES  "); break;
    default             : strcpy(classStr, "          "); break;
  } /* end switch */
} /* end func mjrHDF5_Tclass2str */
