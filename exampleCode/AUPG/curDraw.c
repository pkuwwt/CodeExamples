/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      curDraw.c
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 1999 by Mitch Richling.  All rights reserved.
   @Revision  $Revision$ 
   @SCMdate   $Date$
   @brief     Interactively draw in a terminal window.@EOL
   @Keywords  ncurses
   @Std       C89

   Demonstrates how to draw single characters with various attributes, how to get window max and min coordinates, how to clear the
   screen, and move the cursor around.  In addition, various ncurses concepts are illustrated.
***********************************************************************************************************************************/

#include <ncurses.h>            /* Popular Curses  ????  */
#include <stdlib.h>             /* Standard Lib    C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  int ch, drCh;
  int x, y, maxX, maxY, minX, minY;
  int underline, reverse, bold;

  initscr();
  cbreak();
  noecho();
  keypad(stdscr, TRUE);

  /* We make the cursor very visible.. */
  curs_set(2);

  /* Print the instructions. */
  printw("To Move:    arrow keys\n");
  printw("To Clear:   'c'\n");
  printw("Underline:  'u'\n");
  printw("Reverse:    'r'\n");
  printw("Bold:       'b'\n");
  printw("Normal:     'n'\n");
  printw("Change Char: Any printable char\n");
  printw("To Quit:    'q'\n");
  refresh();

  /* Macro to get minX and minY */
  getbegyx(stdscr, minY, minX);

  /* Macro to get maxX and maxY */
  getmaxyx(stdscr, maxY, maxX);

  /* Set x and y to the center of the screen */
  x = (minX+maxX)/2;
  y = (minY+maxY)/2;

  /* Print our initial character. */
  drCh = '*';
  mvaddch(y, x, drCh);

  /* Move the cursor to x,y */
  move(y, x);

  /* Loop for input */
  underline = reverse = bold = 0;
  while(1) {
    ch = getch();
    switch(ch) {
      case KEY_DOWN      : y=y+1;                                    break;
      case KEY_UP        : y=y-1;                                    break;
      case KEY_LEFT      : x=x-1;                                    break;
      case KEY_RIGHT     : x=x+1;                                    break;
      case 'c'           : clear();                                  break;
      case 'u'           : underline = abs(underline - A_UNDERLINE); break;
      case 'r'           : reverse   = abs(reverse   - A_REVERSE);   break;
      case 'b'           : bold      = abs(bold      - A_BOLD);      break;
      case 'n'           : underline = reverse = bold = 0;           break;
      case 'q'           : endwin(); exit(1);                        break;
      default            : drCh = ch;                                break;
    } /* end switch */
    /* Clip x and y to fit the window. */
    x=(x<minX?minX:x); x=(x>maxX?maxX:x);
    y=(y<minY?minY:y); y=(y>maxY?maxY:y);
    /* Draw things */
    mvaddch(y, x, drCh | underline | reverse | bold);
    move(y, x);
    refresh();
  } /* end while */

  /* We never get here, but we still "return" to quiet the compiler */
  return 0;
} /* end func main */
