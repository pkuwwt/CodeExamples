/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      rlEXH.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     Simple use of readline@EOL
   @Keywords  readline
   @Std       C89

   Adds more history functionality to the rlEX.c example.  In particular, supports history files and expansion of things like '!!'.
***********************************************************************************************************************************/

#include <stdio.h>              /* I/O lib         C89   */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <string.h>             /* Strings         C89   */
#include <readline/readline.h>  /* readline              */
#include <readline/history.h>   /* readline history      */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  char *rLine, goodCmd[1024];
  int i;
  int historyExpandResult;
  char promptString[1024];
  char *rLineExpand;

  /* Set our name so users can configure .inputrc to have special behavior just for this program... */
  rl_readline_name = "rlEX";

  /* Initialize the history system. */
  using_history();

  /* Read in some history from a history file -- omit if support for a history file is not required. */
  if(read_history("rlEX_history")) 
    printf("WARNING: Could not read history file (rlEX_history)\n");

  /* Get rid of the default tab completion of file names -- no need to do this if file name tab completion is appropriate for your
     application. */
  rl_bind_key ('\t', rl_insert);

  /* Loop for for input.  A prompt will be displayed with the current line number for the input, and that input will be echoed back.
     All this with the various readline magic -- up arrow history, etc... */
  for(i=1; ;i++) {
    sprintf(promptString, "Give me some text(%d): ", i);
    rLine = readline(promptString);
    /* Now we expand history substitutions (stuff like '!!') in the input string. */
    historyExpandResult = history_expand(rLine, &rLineExpand);
    goodCmd[0] = '\0';
    switch(historyExpandResult) {
      case 0 : // No useful expansion
        add_history(rLine);
        printf("Line input(%d): '%s'\n", i, rLine);
        strncpy(goodCmd, rLine, 1000);
        break;
      case 1 : // Had an expansion
        printf("Line input(%d): '%s' => '%s'\n", i, rLine, rLineExpand);
        strncpy(goodCmd, rLineExpand, 1000);
        break;
      case -1 : // ERROR of some sort -- error message is in rLineExpand
        printf("ERROR: %s\n", rLineExpand);
        break;
      case 2 : // Expand, but don't RUN (just print like with :p)
        printf("Line input(%d): '%s' => PRT: '%s'\n", i, rLine, rLineExpand);
        break;
    } /* end switch */

    /* You MUST free up the strings that readline() and history_expand() return! */
    free(rLineExpand);
    free(rLine);

    /* Check for recognized commands. */
    if( !(strcmp(goodCmd, "quit"))) {
      /* Write out our history file -- omit if history file support is not needed.  See also append_history,
         history_truncate_file, */
      if(write_history("rlEX_history")) 
        printf("WARNING: Could not write history file\n");
      return 0;
    } /* end if */
  } /* end for */
} /* end func main */
