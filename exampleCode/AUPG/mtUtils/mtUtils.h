 /**
   @file      mtUtils.h
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     Basic pthread programming@EOL
   @Keywords  UNIX pthreads POSIX mutex
   @Std       ISOC POSIX UNIX98 BSD4.3 SYSV3

              This header file us used to provide several useful
			  definitions for multi-threaded programs. C++ support is
			  not complete.

   @Build     
              - Solaris 2.8: use -lpthread -lrt

   @Tested    
              - Solaris 2.8
              - MacOS X.2        
              - Linux (RH 7.3)
*/

/* Protection from multiple inclusions. */
#ifndef I_MTUTILS_H
#define I_MTUTILS_H 1

/* ************************************************************************** */
/** Control access to STDOUT. */
extern pthread_mutex_t console_mutex;

/* ************************************************************************** */
/** This simple function mallocs space for an integer, points the
	given pointer to the space, and then copies the integer (second
	argument) into that space. */
int mallocNsetInt(int **toSet, int value);

/* ************************************************************************** */
/** A simple function that provides a safe alternative to printf.
   POSIX specifies that printf is mt-safe; however, I have had
   problems with it even on modern systems like Solaris 9, MacOS 10.2,
   and RedHat 8.0 (comment updated 2003-10-12). This function will
   have the same return values as printf and will have the same impact
   on errno.  This is a simple wrapper using vprintf. */
int mtPrintf(const char* format, ...);

/* ************************************************************************** */
/** The type for a function used as a thread */
#if __cplusplus
extern "C" { typedef void*  PTHRFUNC(void*); }
#else
typedef void*  PTHRFUNC(void*);
#endif

#endif
