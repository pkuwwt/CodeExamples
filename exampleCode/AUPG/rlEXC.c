/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      rlEXC.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     word completion with GNU readline.@EOL
   @Keywords  gnu readline command word complete completion
   @Std       C89

   Adds custom tab completion to the rlEX.c example.
***********************************************************************************************************************************/

#include <stdio.h>              /* I/O lib         C89   */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <string.h>             /* Strings         C89   */
#include <readline/readline.h>  /* readline              */
#include <readline/history.h>   /* readline history      */

/**********************************************************************************************************************************/
char *completGen(const char *, int);
char **ourCompletor(const char *, int, int);

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  char *rLine;
  int i;
  char promptString[1024];

  /* Set our name so users can configure .inputrc to have special behavior just for this program... */
  rl_readline_name = "rlEX";

  /* Initialize the history system. */
  using_history();

  /* We want to have first go at all completion attempts. */
  rl_attempted_completion_function = ourCompletor;

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

/**********************************************************************************************************************************/
/* Our completer "chains" to the system one -- file name based.  It checks to see if it is completing at the start of a line.  If
   it is the start, then it works. Otherwise, it passes off to the system version.*/
char **ourCompletor(const char *text, int start, int end) {
  char **ourMatches;
  ourMatches = (char **) NULL;
  if(start == 0)
    ourMatches = rl_completion_matches(text, completGen);
  return ourMatches;
} /* end func ourCompletor */

/**********************************************************************************************************************************/
/* All completGen needs to do is return the next match upon each call. */
char *completGen(const char *text, int state) {
  static int textLen;        // Length of text
  char *retWord;             // Word we malloc and return
  static char **curWord;     // The word we check next
  char *ourWords[] = {"my",  // Our list of words     
                      "dog", 
                      "likes", "to",  "pee", "on", "posies",
                      NULL};

  /* state==0 for the first of a new chain of requests.  We use it to tell us when to initialize things. */
  if (!state) {
    curWord = ourWords;
    textLen = strlen(text);
  } /* end if */

  /* Return the next one that matches *text. */
  for(retWord=NULL;(*curWord!=NULL)&&(retWord==NULL); curWord++) {
    if( !(strncmp(*curWord, text, textLen))) {
      /* We MUST malloc retWord as our caller will attempt to free it. */
      if(NULL == (retWord=(char *)malloc(strlen(*curWord)+1))) {
        printf("ERROR: Could not malloc.\n");
        exit(1);
      } /* end if */
      strcpy(retWord, *curWord);
    } /* end if */
  } /* end for */

  return retWord;
} /* end func completGen */
