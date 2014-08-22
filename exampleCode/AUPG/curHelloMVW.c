/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      curHelloMVW.c
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 1999 by Mitch Richling.  All rights reserved.
   @brief     minimal ncurses program demonstrating mvwprintw@EOL
   @Keywords  ncurses
   @Std       C89

   Minimal ncurses program demonstrating the mvwprintw function.
***********************************************************************************************************************************/

#include <ncurses.h>            /* Popular Curses  ????  */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  initscr();                                                          /* Initialize curses (the SCReen)               */
  mvwprintw(stdscr, 10, 40, "Hello, World! (press any key to quit)"); /* Print "Hello, World!" to 'stdscr' at (40,10) */
  refresh();                                                          /* Draw 'stdscr' on the real screen             */
  getch();                                                            /* Wait for a key press                         */
  endwin();                                                           /* Shutdown curses                              */
  return 0;                                                           /* We are done, time to exit.                   */
} /* end func main */
