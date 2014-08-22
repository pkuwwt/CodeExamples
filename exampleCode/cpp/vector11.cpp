// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      vector11.cpp
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 2000,2013,2014 by Mitch Richling.  All rights reserved.
   @brief     Example of some C++11 goodness for STL vectors.@EOL
   @Keywords  Standard C++11 STL vector
   @Std       C++11
***********************************************************************************************************************************/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#include <vector>                  /* STL vector              C++98/11 */ 
#include <iostream>                /* C++ iostream            C++98/11 */
#include <algorithm>

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int main() {
  // You can now initialize a vector with a list
  std::vector<int> demoVec04 = {0, 1, 2, 3, 4};

  /* We don't need to use iterators to traverse a vector: */
  std::cout << "The vector: ";
  for(auto x : demoVec04) 
    std::cout << x << " ";
  std::cout << std::endl;

  // We can also assign with a list
  demoVec04 = {4, 3, 2, 1, 0};

  /* We can also traverse by refrence: */
  for(auto &x : demoVec04) 
    x *= 2;

  /* We can print it out with a lambda and for_each too: */
  std::cout << "The vector: ";
  std::for_each(demoVec04.begin(), demoVec04.end(), [] (int x) { std::cout << x << " "; });
  std::cout << std::endl;

} // end main
