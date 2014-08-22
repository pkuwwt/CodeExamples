// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      set.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 2014 by Mitch Richling.  All rights reserved.
   @brief     Example of how to use SETs in the STL specified by C++ 98 .@EOL
   @Keywords  Standard C++ I/O iostream STL set template
   @Std       C++98

   This program is an example of how use the set template in the STL of C++ 98.
***********************************************************************************************************************************/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#include <set>                     /* STL set                 C++98/11 */
#include <iostream>                /* C++ iostream            C++98/11 */

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int main() {

  std::set<int> mySet;

  // You put stuff in a set with the insert() method.
  mySet.insert(5);
  mySet.insert(2);
  mySet.insert(1);
  mySet.insert(2);  // '2' is already in the set so this has no impact
  mySet.insert(4);

  // We can traverse the whole thing with an iterator.  Note that this will be in sorted order (by key) useing the compare of
  // mySet (less<> by default).
  std::cout << "All the elements of the set: ";
  for(std::set<int>::iterator mySetIter = mySet.begin(); mySetIter != mySet.end(); ++mySetIter)
    std::cout << *mySetIter << " ";
  std::cout << std::endl;

  // One way to check if something is an element of a set is with count().  Keys must be unique, so count() can only be 0 or 1.
  for(int i=1; i<6; i++)
    std::cout << "Looking for " << i << "... " << (mySet.count(i)?"Found":"Not found") << std::endl;
} // end func main
