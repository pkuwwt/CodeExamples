// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      simpleButton.cpp
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 2003 by Mitch Richling.  All rights reserved.
   @brief     Simple button example using no OO@EOL
   @Keywords  oo fltk button
   @Std       C++98 FLTKv1.3
***********************************************************************************************************************************/

/**********************************************************************************************************************************/
#include <FL/Fl.H>              /* FLTK main       FLTK  */
#include <FL/Fl_Window.H>       /* FLTK window     FLTK  */
#include <FL/Fl_Button.H>       /* FLTK button     FLTK  */

#include <stdio.h>              /* I/O lib         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */

/**********************************************************************************************************************************/
static void daButCall(Fl_Widget *w, void *uData) {
  printf("The button was pressed\n");
}

/**********************************************************************************************************************************/
int main() {
  Fl_Window win(500, 70, "Simple Button Demo Program");
  Fl_Button but(20, 20, 460, 30, "Test Button");
  but.callback(daButCall, (void *)0);
  win.show();
  return Fl::run();
}

