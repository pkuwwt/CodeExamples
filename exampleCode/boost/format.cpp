// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
   @file      format.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 2011 by Mitch Richling.  All rights reserved.
   @brief     printf-like formatting for C++ via the boost format.hpp library.@EOL
   @Keywords  none
   @Std       C++98
              
*/

//----------------------------------------------------------------------------------------------------------------------------------

#include <iostream>                /* C++ iostream            C++98/11 */
#include <string>                  /* C++ strings             C++98/11 */
#include <boost/format.hpp>                          /* Print formatting        Boost  */

int main() {
  std::cout << boost::format("Float .................. %7.2f")  % 3.141592653589793F    << std::endl; 
  std::cout << boost::format("Double ................. %7.2lf") % 3.141592653589793L    << std::endl; 

  std::cout << boost::format("Integer ................ %7d")    % 31                    << std::endl; 
  std::cout << boost::format("Integer ................ %07d")   % 31                    << std::endl; 

  std::cout << boost::format("Long Integer ........... %7ld")   % 31L                   << std::endl; 
  std::cout << boost::format("Long Integer ........... %07ld")  % 31L                   << std::endl; 

  std::cout << boost::format("Unsigned Integer ....... %7u")    % 31U                   << std::endl; 
  std::cout << boost::format("Unsigned Long Integer .. %07lu")  % 31UL                  << std::endl; 

  std::cout << boost::format("String ................. %7s")    % "hello"               << std::endl; 
  std::cout << boost::format("String ................. %-7s")   % "hello"               << std::endl; 

  std::cout << boost::format("String ................. %7s")    % std::string("hello")  << std::endl; 
  std::cout << boost::format("String ................. %-7s")   % std::string("hello")  << std::endl; 

}
