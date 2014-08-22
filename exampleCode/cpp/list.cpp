// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      list.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 2000 by Mitch Richling.  All rights reserved.
   @brief     Example of how to use lists in the STL specified by C++ 98 .@EOL
   @Keywords  Standard C++ I/O iostream list STL template
   @Std       C++98
***********************************************************************************************************************************/

#include <list>                    /* STL list                C++98/11 */
#include <iostream>                /* C++ iostream            C++98/11 */
#include <algorithm>               /* STL algorithm           C++98/11 */
#include <ostream>                 /* C++ ostream             C++98/11 */

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void printList(char const* mesg, std::list<int> &L);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int main() {

  std::list<int> demoList01;
  std::list<int> demoList02;
  std::list<int>::iterator demoList01Iter;

  // You can push onto the front and back of a list in constant time:
  demoList01.push_front(1);
  demoList01.push_front(2);
  demoList01.push_back(4);
  demoList01.push_back(5);

  demoList01.insert(++demoList01.begin(), 10);

  // Note that we use the STL <algorithm> *find*, and not some fast version designed for the list type as it dosen't have one!
  demoList01Iter = find(demoList01.begin(), demoList01.end(), 5);
  if(demoList01Iter != demoList01.end()) {
    std::cout << "Found 5 in the list" << std::endl;
    demoList01.insert(demoList01Iter, 20);
  }

  std::cout << "The list has "   << demoList01.size( ) << " elements" << std::endl;
  std::cout << "First Element: " << demoList01.front() << std::endl;
  std::cout << "Last Element:  " << demoList01.back()  << std::endl;
  printList("The elements of the list:     ", demoList01);

  // This is how to remove items...
  demoList01.erase(demoList01.begin());
  demoList01Iter = demoList01.end();
  demoList01Iter--;
  demoList01Iter--;
  demoList01.erase(demoList01Iter, demoList01.end());
  printList("The elements after erase():   ", demoList01);

  // The koolest feature of lists is the way you can splice them together.
  for(int i=10;i<15;i++)
    demoList02.push_front(i);
  demoList01Iter = demoList01.begin();
  ++demoList01Iter;
  ++demoList01Iter;
  demoList01.splice(demoList01Iter, demoList02, demoList02.begin(), demoList02.end());
  printList("The elements after splice():  ", demoList01);

  // unique() removes consecutive elements that are equal:
  demoList01.reverse();
  printList("The elements after reverse(): ", demoList01);

  // list has a fast sort, so don't use the one in <algorithm>
  demoList01.sort();
  printList("The elements after sort():    ", demoList01);

  demoList01.unique();
  printList("The elements after unique():  ", demoList01);
} // end func main

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// This little function demonstrates one way to traverse, and print, a list.
void printList(char const *mesg, std::list<int> &L) {
  std::list<int>::iterator Li;
  std::cout << mesg;
  for(Li = L.begin(); Li != L.end(); ++Li)
    std::cout << *Li << " ";
  std::cout << std::endl;

}
