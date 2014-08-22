/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      curHelloW.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1999 by Mitch Richling.  All rights reserved.
   @brief     Minimal ncurses program demonstrating wprintw@EOL
   @Keywords  ncurses
   @Std       C89

   Minimal ncurses program demonstrating the wprintw function.              
***********************************************************************************************************************************/

#include <ncurses.h>            /* Popular Curses  ????  */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  initscr();                                                 /* Initialize curses (the SCReen)    */
  wprintw(stdscr, "Hello, World! (press any key to quit)");  /* Print "Hello, World!" to 'stdscr' */
  refresh();                                                 /* Draw 'stdscr' on the real screen  */
  getch();                                                   /* Wait for a key press              */
  endwin();                                                  /* Shutdown curses                   */
  return 0;                                                  /* We are done, time to exit.        */
} /* end func main */
