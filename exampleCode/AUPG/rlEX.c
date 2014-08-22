/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      rlEX.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     Simple use of readline@EOL
   @Keywords  readline
   @Std       C89

   Minimal example showing the most basic readline usage.  The huge usability improvement even such a simple usage pattern can
   impart to an application makes readline inclusion an absolute must for any application with a command line interface.  It is not
   often one gets so much for so little effort.

   This example provides up arrow and C-r functionality, but no history file support or expansion of history macros like !!.  To see
   how to implement such history support, check out rlEXH.c.  To see how to implement custom tab completion, see rlEXC.c.

   This example is compatible with even the most traditional, i.e. limited, versions of readline one is likely to encounter.
***********************************************************************************************************************************/

#include <stdio.h>              /* I/O lib         C89   */
#include <string.h>             /* Strings         C89   */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <readline/readline.h>  /* readline              */
#include <readline/history.h>   /* readline history      */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  char *rLine;
  int i;
  char promptString[1024];

  /* Set our name so users can configure .inputrc to have special behavior just for this program... */
  rl_readline_name = "rlEX";

  /* Initialize the history system. */
  using_history();

  /* Get rid of the default tab completion of file names -- no need to do this if file name tab completion is appropriate for your
     application. */
  rl_bind_key ('\t', rl_insert);

  /* Loop for for input.  A prompt will be displayed with the current line number for the input, and that input will be echoed back.
   All this with the various readline magic -- up arrow history, etc... */
  for(i=1; ;i++) {
    sprintf(promptString, "Give me some text(%d): ", i);
    rLine = readline(promptString);

    /* Check for recognized commands. */
    if( !(strcmp(rLine, "quit")))
      return 0;

    /* You MUST free up the strings that readline() returns! */
    free(rLine);
  } /* end for */
} /* end func main */
