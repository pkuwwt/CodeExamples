/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      isocLimits.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1999 by Mitch Richling.  All rights reserved.
   @brief     ISO C limits.h constants@EOL
   @Keywords  UNIX limits ISOC POSIX
   @Std       ISOC
   @Tested    
              - MacOS X.3
              - Debian 7.4 + gcc (Debian 4.7.2-5) 4.7.2

   This demonstrates some of the constants required by ISO C in the limits.h include file.  This stuff is something that
   most UNIX programmers end up needing at some point; however, it has little to do with UNIX system programming....
***********************************************************************************************************************************/

#include <stdio.h>              /* I/O lib         C89   */
#include <limits.h>             /* uname           POSIX */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  printf("char:\n");
  printf(" bare ............. size: %2d range: [%d,%d] (%s,%d-bit)\n", 
                                                           (int)sizeof(char),
                                                           (int)CHAR_MIN,
                                                           (int)CHAR_MAX,
                                                           (CHAR_MIN<0?"SIGNED":"UNSIGNED"),
                                                           (int)CHAR_BIT); 
  printf(" signed ........... size: %2d range: [%d,%d]\n", (int)sizeof(signed char),
                                                           (int)SCHAR_MIN, 
                                                           (int)SCHAR_MAX); 
  printf(" unsigned ......... size: %2d range: [%u,%u]\n", (int)sizeof(unsigned char),
                                                           (unsigned int)0,
                                                           (unsigned int)UCHAR_MAX);

  printf("short int:\n");
  printf(" signed ........... size: %2d range: [%d,%d]\n", (int)sizeof(signed short int),
                                                           (int)SHRT_MIN,
                                                           (int)SHRT_MAX); 
  printf(" unsigned ......... size: %2d range: [%u,%u]\n", (int)sizeof(unsigned short int),
                                                           (unsigned)0,
                                                           (unsigned)USHRT_MAX); 

  printf("int:\n");
  printf(" signed  .......... size: %2d range: [%d,%d]\n", (int)sizeof(signed int),               
                                                           (int)INT_MIN,  
                                                           (int)INT_MAX); 
  printf(" unsigned  ........ size: %2d range: [%u,%u]\n", (int)sizeof(unsigned int),            
                                                           (unsigned)0,         
                                                           (unsigned)UINT_MAX); 

  printf("long int:\n");
  printf(" signed ........... size: %2d range: [%ld,%ld]\n", (int)sizeof(signed long int),
                                                             (long)LONG_MIN,  
                                                             (long)LONG_MAX); 
  printf(" unsigned ......... size: %2d range: [%lu,%lu]\n", (int)sizeof(unsigned long int),        
                                                             (unsigned long)0,         
                                                             (unsigned long)ULONG_MAX); 

  printf("long long int:\n");
  printf(" signed  .......... size: %2d range: [%lld,%lld]\n", (int)sizeof(signed long long int),   
                                                               (signed long long int)LLONG_MIN, 
                                                               (signed long long int)LLONG_MAX); 
  printf(" unsigned ......... size: %2d range: [%llu,%llu]\n", (int)sizeof(unsigned long long int), 
                                                               (unsigned long long int)0,
                                                               (unsigned long long int)ULLONG_MAX); 

  printf("Floating Point Types:\n");
  printf(" float ............ size: %2d\n", (int)sizeof(float)); 
  printf(" double ........... size: %2d\n", (int)sizeof(double)); 
  printf(" long double ...... size: %2d\n", (int)sizeof(long double)); 

  printf("pointer2void ...... size: %2d\n",(int)sizeof(void*)                );

  return 0;
} /* end func main */
