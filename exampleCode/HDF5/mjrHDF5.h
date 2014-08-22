/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/* ****************************************************************************************************************************** */
/**
   @file      mjrHDF5.h
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1997 by Mitch Richling.  All rights reserved.
   @brief     hello   @EOL
   @Keywords  none
   @Std       C89

   Some simple helper functions to make life with HDF5 a little bit less tedious.  For more details, see the mjrHDF5.h file.
              
*/
/* ------------------------------------------------------------------------------------------------------------------------------ */

/* ****************************************************************************************************************************** */
#ifndef INC_MJRHDF5
#define INC_MJRHDF5 1

/* ****************************************************************************************************************************** */
#include <hdf5.h>               /* HDF5 files            */
#include <stdio.h>              /* I/O lib         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */
#include <string.h>             /* Strings         ISOC  */

/* ****************************************************************************************************************************** */
/* Put attributes:  
    mjrHDF5_put_att_arry --------- array of fixed or variable sized data
    mjrHDF5_put_att_sclr --------- Single, scalar value
*/
herr_t mjrHDF5_put_att_arry(hid_t objectID, hid_t attr_type, char *name, void *values, hid_t value_type, hsize_t rank);
herr_t mjrHDF5_put_att_sclr(hid_t objectID, hid_t attr_type, char *name, void *value,  hid_t value_type);

/* ****************************************************************************************************************************** */
/* String Attribute Tools:
    mjrHDF5_put_att_oneFCstr ----- Single, fixed length String (null terminated)
    mjrHDF5_put_att_arrVCstr ----- List (1D array) of variable length Strings (each null terminated)
*/
herr_t mjrHDF5_put_att_oneFCstr(hid_t objectID, char *name, char  *value);
herr_t mjrHDF5_put_att_arrVCstr(hid_t objectID, char *name, char **values, hsize_t rank);

/* ****************************************************************************************************************************** */
/* Global String Attribute Tools:
    mjrHDF5_put_gblAtt_oneFCstr -- Global, Single, Fixed length String (null terminated)
*/
herr_t mjrHDF5_put_gblAtt_oneFCstr(hid_t fileID, char *name, char *value);

/* ****************************************************************************************************************************** */
/* Misc Stuff
    mjrHDF5_chkError ------------- Check error status, print an error message, and exit.
    mjrHDF5_Tclass2str ----------- Convert a H5T_class_t to a string
*/
void mjrHDF5_chkError(int status);
void mjrHDF5_Tclass2str(H5T_class_t class, char *classStr);

#endif
