/**
   @file      fits2tga.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     Convert FITS image to TGA@EOL
   @Keywords  convert fits image tga
   @Std       C89

              This little example uses cfitsio to read in a fits file,
              and write out a 24-bit TGA file.  It understands the two
              most widely used ways to embed color images inside of
              FITS.

              It converts image data to a double, and thus works with
              just about any image data type.  I have included
              commented out code illustrating how to read a SHORT_IMG.
*/

#include <string.h>             /* Strings         ISOC  */
#include <stdio.h>              /* I/O lib         ISOC  */
#include "fitsio.h"

#include "fitsUtil.h"

#define FITS_READ_T double
//#define FITS_READ_T unsigned short

int main(int argc, char *argv[]) {
  int lengthThreeAxis, xLen, yLen;
  int i;
  int numAxis;
  long axLengths[3];
  long fpixel[3], numPixels;
  fitsfile *fitsFilePtr;         
  int status;
  int kwvBITPIX;
  int x, y;
  FITS_READ_T *imageArray;

  status = 0; /* Must initialize status before use */

  /* Open the fits file */
  fits_open_file(&fitsFilePtr, argv[1], READONLY, &status);
  reportAndExitOnFITSerror(status);

  /* Display the data type in the image. */
  fits_get_img_type(fitsFilePtr, &kwvBITPIX, &status);
  reportAndExitOnFITSerror(status);
  switch(kwvBITPIX) {
    case BYTE_IMG   : fprintf(stderr, "Image Type:  8-bit byte pixels, 0 - 255\n");  break;
    case SHORT_IMG  : fprintf(stderr, "Image Type: 16 bit integer pixels\n");        break;
    case LONG_IMG   : fprintf(stderr, "Image Type: 32-bit integer pixels\n");        break;
    case FLOAT_IMG  : fprintf(stderr, "Image Type: 32-bit floating point pixels\n"); break;
    case DOUBLE_IMG : fprintf(stderr, "Image Type: 64-bit floating point pixels\n"); break;
    default         : fprintf(stderr, "Image Type: UNKNOWN\n");                      break;
  }

  /* Get the axis count for the image */
  fits_get_img_dim(fitsFilePtr, &numAxis,  &status);
  reportAndExitOnFITSerror(status);
  fprintf(stderr, "Number of axis: %d\n", numAxis);

  /* Find the x/y-axis dimensions and the color dimension if it exists. */
  if(numAxis < 2) {
    fprintf(stderr, "Too few axes to be a real image!\n");
    exit(1);
  } else if(numAxis > 3) {
    fprintf(stderr, "Too many axes to be a real image!\n");
    exit(2);
  }

  /* Get the size of each axis */
  fits_get_img_size(fitsFilePtr, 3, axLengths, &status);
  reportAndExitOnFITSerror(status);

  /* Find the color axis if it exists.. */
  if(numAxis == 2) {
    lengthThreeAxis = 0;
    xLen = axLengths[1-1];
    yLen = axLengths[2-1];
  } else { // (numAxis == 3)
    if(axLengths[3-1] == 3) {
      lengthThreeAxis = 3;
      xLen = axLengths[1-1];
      yLen = axLengths[2-1];
    } else if(axLengths[1-1] == 3) {
      lengthThreeAxis = 1;
      xLen = axLengths[2-1];
      yLen = axLengths[3-1];
    } else {
      fprintf(stderr, "Found 3 axis, but can't figure out RGB dimension!\n");
      exit(3);
    }
  }

  /* Compute the number of pixels */
  numPixels = xLen * yLen;
  if(lengthThreeAxis)
    numPixels = numPixels * 3;

  /* Report on image size and color axis location */
  if(lengthThreeAxis)
    fprintf(stderr, "%dx%d Color FITS image.  RGB is ax %d\n", xLen, yLen, lengthThreeAxis);
  else
    fprintf(stderr, "%dx%d FITS image.\n", xLen, yLen);

  /* Set up fpixel for a full image read. */
  for(i=1; i<=numAxis; i++) 
    fpixel[i-1] = 1;

  /* Allocate space used for the image. */
  imageArray = (FITS_READ_T *)malloc(sizeof(FITS_READ_T)*(numPixels+1));

  /* Read in the data in one big gulp */
  fits_read_pix(fitsFilePtr, TDOUBLE, fpixel, numPixels, NULL, imageArray, NULL, &status);
  //fits_read_pix(fitsFilePtr, TUSHORT, fpixel, numPixels, NULL, imageArray, NULL, &status);

  /* Compute maximum pixel value */
  FITS_READ_T maxF =  imageArray[0];
  FITS_READ_T minF =  imageArray[0];
  for(i=1; i<numPixels; i++) {
    if(imageArray[i] > maxF) maxF = imageArray[i];
    if(imageArray[i] < minF) minF = imageArray[i];
  }
  fprintf(stderr, "pix range: [%f, %f]\n", (double)minF, (double)maxF);

  /* Print out the image file */
  /* Write the TGA header */
  putchar(0); 					/* idlength */
  putchar(0); 					/* colourmaptype */
  putchar(2); 					/* datatypecode */
  putchar(0); 					/* 16-bit colourmap origin */
  putchar(0);
  putchar(0); 					/* colurmaplength */
  putchar(0);
  putchar(0); 					/* colormapdepth */
  putchar(0); 					/* 16-bit x_origin */
  putchar(0);
  putchar(0); 					/* 16-bit y_origon */
  putchar(0);
  putchar(xLen & 0x00ff);       /* LSB xLen */
  putchar((xLen & 0xff00)/256); /* MSB xLen */
  putchar(yLen & 0x00ff);       /* LSB yLen */
  putchar((yLen & 0xff00)/256); /* MSB yLen */
  putchar(24);                  /* bits per pixel. */
  putchar(0);                   /* imagedescriptor */
  /* Write out the TGA image data */
  for(y=0;y<yLen;y++)
    for(x=0;x<xLen;x++)
      switch(lengthThreeAxis) {
        case 0: putchar(255*(imageArray[x+xLen*y]-minF)/(maxF-minF));
                putchar(255*(imageArray[x+xLen*y]-minF)/(maxF-minF));
                putchar(255*(imageArray[x+xLen*y]-minF)/(maxF-minF));             break;
        case 1: putchar(255*(imageArray[3*(x+xLen*y)+2]-minF)/(maxF-minF));
                putchar(255*(imageArray[3*(x+xLen*y)+1]-minF)/(maxF-minF));
                putchar(255*(imageArray[3*(x+xLen*y)+0]-minF)/(maxF-minF));       break;
        case 3: putchar(255*(imageArray[x+xLen*y+2*xLen*yLen]-minF)/(maxF-minF));
                putchar(255*(imageArray[x+xLen*y+1*xLen*yLen]-minF)/(maxF-minF));
                putchar(255*(imageArray[x+xLen*y+0*xLen*yLen]-minF)/(maxF-minF)); break;
      }

  /* Free up space used for the image. */
  free(imageArray);

  /* We are done.  Close the file. */
  fits_close_file(fitsFilePtr, &status);
  reportAndExitOnFITSerror(status);

  /* That's it.  Time to exit. */
  return(0);
}
