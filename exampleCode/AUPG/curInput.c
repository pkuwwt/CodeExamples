/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      curInput.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1999 by Mitch Richling.  All rights reserved.
   @brief     How to do unbuffered I/O and key identification@EOL
   @Keywords  ncurses
   @Std       C89

   Demonstrates how to do non-buffered input with ncurses and how to identify keys.              
***********************************************************************************************************************************/

#include <ncurses.h>            /* Popular Curses  ????  */
#include <stdlib.h>             /* Standard Lib    C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  int ch;

  initscr();

  /* "unbuffered" terminal mode.  Ctrl-C and it's buddies still work with cbreak().  To change that, use raw().  To get
     semi-asynchronous keyboard I/O, use halfdelay(). */
  cbreak();

  /* We do the echo, we don't need the term to do it for us! */
  noecho();

  /* Enable special keys (like F1 & arrow keys) */
  keypad(stdscr, TRUE);

  /* Print the instructions. */
  printw("Press 'q' to quit!");
  refresh();

  /* Loop for input */
  while(1) {
    ch = getch();
    mvprintw(5, 5, "You pressed: ");
    switch(ch) {
      case KEY_DOWN      : printw("down-arrow key");             break;
      case KEY_UP        : printw("up-arrow key");               break;
      case KEY_LEFT      : printw("left-arrow key");             break;
      case KEY_RIGHT     : printw("right-arrow key");            break;
      case KEY_HOME      : printw("home key");                   break;
      case KEY_BACKSPACE : printw("backspace key");              break;
      case KEY_F(1)      : printw("Value of function key 1");    break;
      case KEY_F(2)      : printw("Value of function key 2");    break;
      case KEY_F(3)      : printw("Value of function key 3");    break;
      case KEY_F(4)      : printw("Value of function key 4");    break;
      case KEY_F(5)      : printw("Value of function key 5");    break;
      case KEY_F(6)      : printw("Value of function key 6");    break;
      case KEY_F(7)      : printw("Value of function key 7");    break;
      case KEY_F(8)      : printw("Value of function key 8");    break;
      case KEY_F(9)      : printw("Value of function key 9");    break;
      case KEY_F(10)     : printw("Value of function key 10");   break;
      case KEY_F(11)     : printw("Value of function key 11");   break;
      case KEY_F(12)     : printw("Value of function key 12");   break;
      case KEY_DL        : printw("delete-line key");            break;
      case KEY_IL        : printw("insert-line key");            break;
      case KEY_DC        : printw("delete-character key");       break;
      case KEY_CLEAR     : printw("clear-screen or erase key");  break;
      case KEY_EOS       : printw("clear-to-end-of-screen key"); break;
      case KEY_EOL       : printw("clear-to-end-of-line key");   break;
      case KEY_SF        : printw("scroll-forward key");         break;
      case KEY_SR        : printw("scroll-backward key");        break;
      case KEY_NPAGE     : printw("next-page key");              break;
      case KEY_PPAGE     : printw("previous-page key");          break;
      case KEY_ENTER     : printw("enter/send key");             break;
      case KEY_PRINT     : printw("print key");                  break;
      case KEY_LL        : printw("lower-left key (home down)"); break;
      case KEY_A1        : printw("upper left of keypad");       break;
      case KEY_A3        : printw("upper right of keypad");      break;
      case KEY_B2        : printw("center of keypad");           break;
      case KEY_C1        : printw("lower left of keypad");       break;
      case KEY_C3        : printw("lower right of keypad");      break;
      case KEY_END       : printw("end key");                    break;         
      case 'q'           : endwin(); exit(1);         
      default            : printw("%c", ch);                     break;         
    } /* end switch */
    printw("                                 ");
    refresh();
  } /* end while */

  /* We never get here, but we still "return" to quiet the compiler */
  return 0;
} /* end func main */

