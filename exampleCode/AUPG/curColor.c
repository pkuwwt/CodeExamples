/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      curColor.c
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 1999 by Mitch Richling.  All rights reserved.
   @brief     Various concepts related to color use in ncurses are demonstrated.@EOL
   @Keywords  ncurses
   @Std       C89
***********************************************************************************************************************************/

#include <ncurses.h>            /* Popular Curses  ????  */
#include <stdlib.h>             /* Standard Lib    C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {   
  int colX, colY, colIdx;

  /* Initialize curses */
  initscr();

  /* Figure out if we have color support */
  if(has_colors() == FALSE) {
    endwin();
    printf("ERROR: This demo requires a terminal that has color support.\n");
    exit(1);
  } /* end if */

  printw("All the colors you could want:\n\n");

  /* Start up the color system */
  start_color();

  /* Most curses implementations define eight named colors:
      - COLOR_BLACK  - COLOR_RED      - COLOR_GREEN  - COLOR_YELLOW
      - COLOR_BLUE   - COLOR_MAGENTA  - COLOR_CYAN   - COLOR_WHITE
     They have values from 0 to 7 inclusive.  The following loops generate all 64 possible colors and draws some text in each
     color. */
  colIdx=0;
  for(colX=0; colX<8; colX++) {
    for(colY=0; colY<8; colY++) {
      init_pair(colIdx, colX, colY);
      attron(COLOR_PAIR(colIdx));
      printw("####");
      attroff(COLOR_PAIR(colIdx));
      attron(COLOR_PAIR(0));
      printw("  ");
      attroff(COLOR_PAIR(0));
      colIdx++;
    } /* end for */
    printw("\n");
  } /* end for */

  /* Update screen, print a prompt, and wait for a key press. */
  printw("Press any key to quit!");
  refresh();    
  getch();

  /* shutdown curses, and return */
  endwin();
  return 0;
} /* end func main */
