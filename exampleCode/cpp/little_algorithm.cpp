// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      little_algorithm.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 2014 by Mitch Richling.  All rights reserved.
   @brief     Example of how to use algorithms in the STL specified by C++ 98.@EOL
   @Keywords  Standard C++ STL algorithm
   @Std       C++11

   This program is an example of how use the algorithms found in the <algorithms> header file that is part of the STL of C++ 98.

     -  min
     -  max
     -  swap              
***********************************************************************************************************************************/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#include <iostream>                /* C++ iostream            C++98/11 */
#include <algorithm>               /* STL algorithm           C++98/11 */
#include <functional>              /* STL funcs               C++98/11 */

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int main() {

  int i = 1;
  int j = 2; 

  std::cout << "Minimum of " << i << " and " << j << " is: " << std::min(i,j) << std::endl;
  std::cout << "Maximum of " << i << " and " << j << " is: " << std::max(i,j) << std::endl;

  std::cout << "Before val i=" << i << " and j=" << j << std::endl;
  std::swap(i, j);
  std::cout << "After swap i=" << i << " and j=" << j << std::endl;

} // end func main

