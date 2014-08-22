// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      generic.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 2000 by Mitch Richling.  All rights reserved.
   @brief     Generic functions.@EOL
   @Keywords  Standard C++ STL algorithm
   @Std       C++98

   This program is an example of how write some a simple generic function to work with things in the STL.              
***********************************************************************************************************************************/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#include <vector>                  /* STL vector              C++98/11 */ 
#include <list>                    /* STL list                C++98/11 */
#include <iostream>                /* C++ iostream            C++98/11 */
#include <algorithm>               /* STL algorithm           C++98/11 */

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
template <class T> void printSeq(char const* msg, T b, T e);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int main() {

  std::vector<int> demoVec;
  std::list<int> demoLst;

  // First we put something in our vector and our list..
  for(int i=1; i<10; i++) {
    demoVec.push_back(i);
    demoLst.push_back(i);
  }

  // Now call our printSeq function (note we don't need to specify the template  parameter in this case..
  printSeq("vec: ", demoVec.begin(), demoVec.end());
  printSeq("lst: ", demoLst.begin(), demoLst.end());

  // While we are not required to specify the template parameter, we can.  If we did, it would look something like this:
  // printSeq<std::vector<int>::iterator >("vec: ", demoVec.begin(), demoVec.end());
  // printSeq<std::list<int>::iterator   >("lst: ", demoLst.begin(), demoLst.end());
} /* end func main */

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
template <class T> void printSeq(char const* msg, T b, T e) {
     std::cout << msg;
     for(T vi = b; vi != e; ++vi)
       std::cout << *vi << " ";
     std::cout << std::endl;  
}


