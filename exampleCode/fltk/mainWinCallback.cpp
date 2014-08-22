// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      mainWinCallback.cpp
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 2003 by Mitch Richling.  All rights reserved.
   @brief     How to assign the main window callback.   @EOL
   @Keywords  fltk window
   @Std       C++98 FLTKv1.3

   The main window callback is invoked when the close button is hit.  This program illustrates how to bind this callback to a
   function that will quit the application -- this is actually the default behavior.  Use this callback to take care of things
   before an application is closed!
***********************************************************************************************************************************/

/**********************************************************************************************************************************/
#include <FL/Fl.H>              /* FLTK main       FLTK  */
#include <FL/Fl_Window.H>       /* FLTK window     FLTK  */

#include <stdio.h>              /* I/O lib         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */

/**********************************************************************************************************************************/
void daWinCall(Fl_Widget *w, void *uData) {
  printf("The window callback was called.\n");
  exit(0);
}

/**********************************************************************************************************************************/
int main() {
  Fl_Window win(500, 500, "Main Window Callback Example Program");
  win.callback(daWinCall);
  win.show();
  return Fl::run();
}

