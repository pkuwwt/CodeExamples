// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
   @file      intervalA.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
   @brief     Interval arithmetic with boost.@EOL
   @Keywords  none
   @Std       C++98

*/

//----------------------------------------------------------------------------------------------------------------------------------

#include <boost/numeric/interval.hpp>                /* Interval Arithmetic     Boost  */
#include <iostream>                /* C++ iostream            C++98/11 */

int main() {

   boost::numeric::interval<double> a(-1.2, 3.4);
   boost::numeric::interval<double> b;
   boost::numeric::interval<double> c;

   b = boost::numeric::interval<double>(5.4, 12.3);
   c = a * b;

   std::cout << '[' << a.lower() << ", " << a.upper() << ']' << std::endl;
   std::cout << '[' << b.lower() << ", " << b.upper() << ']' << std::endl;
   std::cout << '[' << c.lower() << ", " << c.upper() << ']' << std::endl;

   a = boost::numeric::square(a);
   std::cout << '[' << a.lower() << ", " << a.upper() << ']' << std::endl;

   // many functions exist in the library too:
   //   min max abs square pow root division_part? multiplicative_inverse
   //   intersect hull overlap in zero_in subset proper_subset empty singleton equal
   //   sqrt log exp sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh fmod
   //   add sub mul div

   std::cout << "lower="  << boost::numeric::lower(a)  << std::endl;
   std::cout << "upper="  << boost::numeric::upper(a)  << std::endl;
   std::cout << "median=" << boost::numeric::median(a) << std::endl;
   std::cout << "width="  << boost::numeric::width(a)  << std::endl;
   std::cout << "norm="   << boost::numeric::norm(a)   << std::endl;

}
