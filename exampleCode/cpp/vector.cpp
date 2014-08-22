// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      vector.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 2000 by Mitch Richling.  All rights reserved.
   @brief     Example of how to use vectors in the STL specified by C++ 98.@EOL
   @Keywords  Standard C++ STL vector
   @Std       C++98

   This program is an example of how use the vector template in the STL of C++ 98.

   The std::vector template is variable sized, array-like container -- probably the most used container in the STL.  While
   array-like indexing is allowed, vectors will not automatically grow to make out of bounds indexes valid nor will they do the
   right thing if an element is retrieved with an out of bounds index (this is no doubt a serious disappointment for Perl and Ruby
   programmers) In order to grow a vector, the safe members like push_back() and insert() should be used.

   Methods are also provided to directly manipulate the storage capacity of a vector.
***********************************************************************************************************************************/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#include <vector>                  /* STL vector              C++98/11 */ 
#include <iostream>                /* C++ iostream            C++98/11 */

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int main() {
  // Default construction leads to an empty vector with unknown allocated storage.
  std::vector<int> demoVec01;  

  // You can pre-allocate a vector and fill it with default values:
  std::vector<int> demoVec02(10);

  // You can pre-allocate a vector, and fill it with a specific constant (-1):
  std::vector<int> demoVec03(10, -1);

  // The fastest way to see if a vector is empty is with empty():
  if(demoVec01.empty())
    std::cout << "demoVec01 is empty.  That's how vectors start life..." << std::endl;
  if( !(demoVec02.empty()))
    std::cout << "demoVec02 is NOT empty as we used a constructor..." << std::endl;

  // Insert at the beginning, which is SLOW.  The vector will grow as required.
  for(std::vector<int>::size_type i=0; i<5; i++)
    demoVec01.insert(demoVec01.begin(), i);

  // Insert at the end, which is FAST.  The vector will grow as required.
  for(std::vector<int>::size_type i=6; i<11; i++)
    demoVec01.insert(demoVec01.end(), i);

  // push_back is a more common way to insert at the end (It is FAST too):
  for(std::vector<int>::size_type i=12; i<20; i++) 
    demoVec01.push_back(i);

  // We can assign elements with the [] operator, but never use an index that doesn't exist:
  demoVec01[2] = 30;

  // Space can be allocated with reserve(), and reported with capacity()
  std::cout << "pre-reserve demoVec01 Size:      " << demoVec01.size()     << std::endl;
  std::cout << "pre-reserve demoVec01 Capacity:  " << demoVec01.capacity() << std::endl;
  demoVec01.reserve(demoVec01.capacity()+100);
  std::cout << "post-reserve demoVec01 Size:     " << demoVec01.size()     << std::endl;
  std::cout << "post-reserve demoVec01 Capacity: " << demoVec01.capacity() << std::endl;

  // We can extract a value with the [] operator, but don't use an index that is out of bounds:
  std::cout << "demoVec01[2]=" << demoVec01[2] << std::endl;

  // We can traverse the whole thing, with an iterator, like this:
  std::cout << "All the elements of the vector: ";
  for(std::vector<int>::iterator demoVec01Iter = demoVec01.begin(); demoVec01Iter != demoVec01.end(); ++demoVec01Iter)
    std::cout << *demoVec01Iter << " ";
  std::cout << std::endl;

  // Remove the last value from the vector.  This is a FAST operation.
  std::cout << "Size of demoVec01 before pop: " << demoVec01.size() << std::endl;
  demoVec01.pop_back();
  std::cout << "Size of demoVec01 after pop: "  << demoVec01.size() << std::endl;

  // We can wack the last element with erase().  Note the '-1' used with the random access iterator returned by end().
  demoVec01.erase(demoVec01.end()-1);
  std::cout << "Size of demoVec01 after erase (end): " << demoVec01.size() << std::endl;

  // We can wack the FIRST element as well with erase(), but it is VERY slow.
  demoVec01.erase(demoVec01.begin());
  std::cout << "Size of demoVec01 after erase (begin): " << demoVec01.size() << std::endl;
  
  // We can traverse the whole thing using [] notation:
  std::cout << "All the elements of the vector: ";
  for(std::vector<int>::size_type i=0; i<demoVec01.size(); i++)
    std::cout << demoVec01[i] << " ";
  std::cout << std::endl;

} // end func main
