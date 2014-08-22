/**
   @file      dumpFitsHeader.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1997 by Mitch Richling.  All rights reserved.
   @brief     Dump FITS header@EOL
   @Keywords  dump fits header
   @Std       C89

              Dump the content of the first HUD in a a FITS file file.
*/

#include <string.h>             /* Strings         ISOC  */
#include <stdio.h>              /* I/O lib         ISOC  */
#include "fitsio.h"

#include "fitsUtil.h"

int main(int argc, char *argv[]) {
  fitsfile *fitsFilePtr;         
  char card[FLEN_CARD]; 
  int status, numKey, numHDU, curKey, curHDU, hduType;

  status = 0; /* Must initialize status before use */

  if(argc < 2) {
    printf("ERROR\n");
    exit(11);
  }

  /* Open the fits file */
  fits_open_file(&fitsFilePtr, argv[1], READONLY, &status);
  reportAndExitOnFITSerror(status);

  /* Get the number of HDUs -- usually only one */
  fits_get_num_hdus(fitsFilePtr, &numHDU, &status);
  reportAndExitOnFITSerror(status);
  printf("Number of headers in file: %d\n", numHDU);

  /* Get the current HDU number.  This first one is '1', not '0'. */
  fits_get_hdu_num(fitsFilePtr, &curHDU);
  reportAndExitOnFITSerror(status);
  printf("Working on header number: %d\n", curHDU);

  fits_get_hdu_type(fitsFilePtr,  &hduType, &status);
  reportAndExitOnFITSerror(status);
  switch(hduType) {
    case IMAGE_HDU : printf("HDU Type: IMAGE\n");        break;
    case ASCII_TBL : printf("HDU Type: ASCII Table\n");  break;
    case BINARY_TBL: printf("HDU Type: Binary Table\n"); break;
    default        : printf("HDU Type: UNKNOWN\n");      break;
  }

  /* Get the size (number of keys) of the header space */
  fits_get_hdrspace(fitsFilePtr, &numKey, NULL, &status);
  reportAndExitOnFITSerror(status);
  printf("Number of keys in current HDU: %d\n", numKey);

  /* One can traverse all the lines like this: */
  for(curKey = 1; curKey <= numKey; curKey++)  { 
    /* read the current keyword */
    fits_read_record(fitsFilePtr, curKey, card, &status); 
    reportAndExitOnFITSerror(status);
    printf("%5d: %s\n", curKey, card);
  }
  printf("END\n\n");

  /* We are done.  Close the file. */
  fits_close_file(fitsFilePtr, &status);
  reportAndExitOnFITSerror(status);

  /* If we get here, everything worked! */
  return 0;
}
