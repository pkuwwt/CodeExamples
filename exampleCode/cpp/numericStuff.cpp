// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      numericStuff.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 2000,2014 by Mitch Richling.  All rights reserved.
   @brief     Demo some of the stuff in <numeric>.@EOL
   @Keywords  none
   @Std       C++11
***********************************************************************************************************************************/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#include <iostream>                /* C++ iostream            C++98/11 */
#include <vector>                  /* STL vector              C++98/11 */ 
#include <numeric>                 /* C++ numeric             C++98/11 */

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
std::ostream& operator<< (std::ostream &out, std::vector<int> v) {
  for(auto x : v)
    std::cout << x << " ";
  return out;
} /* end operator<< std::vector<int> */

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int multiply(int x, int y) { return x*y; }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int add(int x, int y)      { return x+y; }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int subtract(int x, int y) { return x-y; }
 
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int main() {

  std::vector<int> v = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  std::vector<int> u = {10, 9, 8, 7, 6, 5, 4, 3, 2,  1};
 
  { int sum = std::accumulate(v.begin(), v.end(), 0);
    std::cout << "sum     " << sum     << std::endl;
  }

  { int product = std::accumulate(v.begin(), v.end(), 1, multiply);
    std::cout << "product " << product << std::endl;
  }

  { int inProd = std::inner_product(v.begin(), v.end(), u.begin(), 0);
    std::cout << "in prod " << inProd << std::endl;
  }

  { int inProd = std::inner_product(v.begin(), v.end(), u.begin(), 0, add, multiply);
    std::cout << "in prod " << inProd << std::endl;
  }

  { std::vector<int> r(v.size(),-1); // Note that r must be big enough!!!
    std::partial_sum(v.begin(), v.end(), r.begin());
    std::cout << r << std::endl;
  }

  { std::vector<int> r(v.size(),-1); // Note that r must be big enough!!!
    std::partial_sum(v.begin(), v.end(), r.begin(), add);
    std::cout << r << std::endl;
  }

  { std::vector<int> r(v.size(),-1); // Note that r must be big enough!!!
    std::adjacent_difference(v.begin(), v.end(), r.begin());
    std::cout << r << std::endl;
  }

  { std::vector<int> r(v.size(),-1); // Note that r must be big enough!!!
    std::adjacent_difference(v.begin(), v.end(), r.begin(), subtract);
    std::cout << r << std::endl;
  }

} // end function main
