// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      algorithm.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 2000 by Mitch Richling.  All rights reserved.
   @brief     Example of how to use algorithms in the STL specified by C++ 98.@EOL
   @Keywords  Standard C++ STL algorithm
   @Std       C++98

   This program is an example of how use some of the algorithms found in the <algorithms> header file in C++ 98.

     - adjacent_find      
     - adjacent_find (w predicate)
     - count                      
     - count_if                   
     - find               
     - find_first_of      
     - find_first_of (w predicate)
     - find_if            
     - max_element                
     - min_element        

***********************************************************************************************************************************/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#include <vector>                  /* STL vector              C++98/11 */ 
#include <iostream>                /* C++ iostream            C++98/11 */
#include <algorithm>               /* STL algorithm           C++98/11 */
#include <functional>              /* STL funcs               C++98/11 */

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void printVec(char const* msg, std::vector<int> &v);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
template <class T> void printSeq(char const* msg, T b, T e);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int main() {
  std::vector<int> v1, v2, v3;
  std::vector<int>::iterator vi;
  int i;

  // push_back can also be used put put things at the end of vector:
  for(int i=0; i<10; i++) {
    v1.push_back(i);
    v2.push_back(i);
	v2.insert(v2.begin(), 2*i);
	v3.insert(v3.begin(), 100-i);
    v3.push_back(i);
  } /* end for */
  printVec("v1: ", v1);
  printVec("v2: ", v2);
  printVec("v3: ", v3);

  // Demo min_element().  max_element() works the same way..
  vi = std::min_element(v3.begin(), v3.end());
  std::cout << "Minimum element in v3: " << *vi << std::endl;

  // You can specify a predicate for both min_element() and max_element():
  vi = std::min_element(v3.begin(), v3.end(), std::greater<int>());
  std::cout << "Minimum element in v3: " << *vi << std::endl;

  vi = std::find(v3.begin(), v3.end(), 95);
  if(vi != v3.end()) 
    std::cout << "Found 95 in v3: " << *vi << std::endl;

  vi = std::find_if(v3.begin(), v3.end(), std::bind2nd(std::greater<int>(), 95));
  if(vi != v3.end()) 
    std::cout << "Found >95 in v3: " << *vi << std::endl;

  i = std::count(v3.begin(), v3.end(), 95);
  std::cout << "Number of 95's in v3: " << i << std::endl;

  i = std::count_if(v3.begin(), v3.end(), std::bind2nd(std::greater<int>(), 95));
  std::cout << "Number of >95 in v3: " << i << std::endl;

  vi = std::adjacent_find(v2.begin(), v2.end());
  if(vi != v2.end()) 
    std::cout << "Found two alike in v2: " << *vi << " and " << *(++vi) << std::endl;

  // Find first pair such that they both are >4..
  vi = std::adjacent_find(v2.begin(), v2.end(), std::less<int>());
  if(vi != v2.end()) 
    std::cout << "Found two next to each other with i<j in v2: " << *vi << " and " << *(++vi) << std::endl;

  printSeq("v1[5:7]: ", v2.begin()+5, v2.begin()+8);

  vi = std::find_first_of(v1.begin(), v1.end(), v2.begin()+5, v2.begin()+8);
  if(vi != v1.end()) 
    std::cout << "Found something in v2[5:7] in v1: " << *vi << std::endl;

  vi = std::find_first_of(v1.begin(), v1.end(), v2.begin()+5, v2.begin()+8, std::greater<int>());
  if(vi != v1.end()) 
    std::cout << "Found something greater than one of v2[5:7] in v1: " << *vi << std::endl;

  vi = std::search(v2.begin(), v2.end(), v1.begin(), v1.end());
  if(vi != v2.end()) {
    std::cout << "Found v1 in v2 at: " << (vi - v2.begin()) << std::endl;
    printSeq("The part of v2 we found: ", vi, v2.end());
  } /* end if */

} /* end func main */

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void printVec(char const* msg, std::vector<int> &v) {
  std::cout << msg;
  for(std::vector<int>::iterator vi = v.begin(); vi != v.end(); ++vi)
 	std::cout << *vi << " ";
  std::cout << std::endl;
} /* end func printVec */

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
template <class T> void printSeq(char const* msg, T b, T e) {
     std::cout << msg;
     for(T vi = b; vi != e; ++vi)
       std::cout << *vi << " ";
     std::cout << std::endl;  
} /* end func printSeq */
