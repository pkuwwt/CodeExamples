// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      multiset.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 2000 by Mitch Richling.  All rights reserved.
   @brief     Example of how to use the multiset in the STL specified by C++ 98 .@EOL
   @Keywords  Standard C++ I/O iostream STL multiset template
   @Std       C++98
***********************************************************************************************************************************/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#include <set>                     /* STL set                 C++98/11 */
#include <iostream>                /* C++ iostream            C++98/11 */

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int main() {

  std::multiset<int> mySet;

  mySet.insert(5);
  mySet.insert(2);
  mySet.insert(1);
  mySet.insert(2);  // '2' is already in the multiset, so this will insert  ANOTHER copy -- putting two of them in the multiset.
                    // This is different from a 'set'.
  mySet.insert(4);

  // We can traverse the whole thing like this: 
  std::cout << "All the elements of the set: ";
  for(std::set<int>::iterator mySetIter = mySet.begin(); mySetIter != mySet.end(); ++mySetIter)
    std::cout << *mySetIter << " ";
  std::cout << std::endl;

  // One way to check if something is an element of a set is with count(). Unlike with set, count can be any non-negative value in a
  // multiset.
  for(int i=1; i<6; i++)
    std::cout << "Looking for " << i << "... Found " << mySet.count(i) << std::endl;
} // end func main
