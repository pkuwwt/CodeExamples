// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      simpleButtonOO.cpp
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 2003 by Mitch Richling.  All rights reserved.
   @brief     Simple button example with objects@EOL
   @Keywords  oo button fltk
   @Std       C++98 FLTKv1.3
***********************************************************************************************************************************/

/**********************************************************************************************************************************/
#include <FL/Fl.H>              /* FLTK main       FLTK  */
#include <FL/Fl_Window.H>       /* FLTK window     FLTK  */
#include <FL/Fl_Button.H>       /* FLTK button     FLTK  */

#include <stdio.h>              /* I/O lib         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */

/**********************************************************************************************************************************/
class myWin : public Fl_Window {
  private:
    static void daButCall(Fl_Widget *w, void *uData) {
      printf("The button was pressed\n");
    }

  public:
    myWin(int w, int h, const char *name=0) : Fl_Window(w, h, name) {
      Fl_Button *but = new Fl_Button(20, 20, 460, 30, "Test Button");
      but->callback(daButCall, (void *)0);
      show();
    }
};

/**********************************************************************************************************************************/
int main() {
  myWin win(500, 70, "Hello (Title)");
  return Fl::run();
}

