// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      sort.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
   @brief     Sort C++11 style.@EOL
   @Keywords  c++ stl c++11 2011 lambda functor sort algorithm
   @Std       C++11
***********************************************************************************************************************************/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#include <iostream>                /* C++ iostream            C++98/11 */
#include <algorithm>               /* STL algorithm           C++98/11 */
#include <vector>                  /* STL vector              C++98/11 */ 
using namespace std;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
bool cmpFunction (int i,int j) { return (i<j); }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
struct cmpFunctorC {
  bool operator() (int i,int j) { return (i<j);}
} cmpFunctor;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
vector<int> intVect;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void printV98(void);
void printV11(void);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int main () {

  cout << endl;
  intVect = {1,3,5,7,9,8,6,4,2,0};
  printV98();
  sort (intVect.begin(), intVect.begin());                                       // If int were a class with an < operator, that
  printV98();                                                                    // would be used here.

  cout << endl;
  intVect = {1,3,5,7,9,8,6,4,2,0};
  printV98();
  sort (intVect.begin(), intVect.end(), cmpFunction);                            // Use a comparison function
  printV98();

  cout << endl;
  intVect = {1,3,5,7,9,8,6,4,2,0};
  printV98();
  sort (intVect.begin(), intVect.end(), cmpFunctor);                             // Use a functor
  printV98();

  cout << endl;
  intVect = {1,3,5,7,9,8,6,4,2,0};
  printV11();
  sort (intVect.begin(), intVect.end(), [](int i, int j) { return (i < j); });   // Use a lambda (C++ 11)
  printV11();

  return 0;

} // end func main

//----------------------------------------------------------------------------------------------------------------------------------
// This is the C++ 98 way of traversing a container
void printV98(void) {
  cout << "Vector: ";
  for (vector<int>::iterator it=intVect.begin(); it!=intVect.end(); ++it)
    cout << *it << " ";
  cout << endl;
}

//----------------------------------------------------------------------------------------------------------------------------------
// This is the C++ 11 way of traversing a container
void printV11(void) {
  cout << "Vector: ";
  for_each (intVect.begin(), intVect.end(), [](int i) { cout << i << " "; });
  cout << endl;
}
