// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      complex.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 2000 by Mitch Richling.  All rights reserved.
   @brief     Complex number types in STL. @EOL
   @Keywords  complex stl math
   @Std       C++98

   One must be careful with this type as some standard library implementations have not paid much attention to performance for
   complex numbers -- this isn't FORTRAN after all...
              
***********************************************************************************************************************************/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#include <iostream>                /* C++ iostream            C++98/11 */
#include <complex>                 /* STL algorithm           C++98/11 */

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int main(void) {

  std::complex<float> c(1,2);
  std::complex<float> zero = std::complex<float>(0,0);

   std::cout << "zero                    :: " <<  zero                    << std::endl;
   std::cout << "c                       :: " <<  c                       << std::endl;
   std::cout << "std::sin(c)             :: " <<  std::sin(c)             << std::endl; 
   std::cout << "std::sinh(c)            :: " <<  std::sinh(c)            << std::endl; 
   std::cout << "std::cos(c)             :: " <<  std::cos(c)             << std::endl; 
   std::cout << "std::cosh(c)            :: " <<  std::cosh(c)            << std::endl; 
   std::cout << "std::tan(c)             :: " <<  std::tan(c)             << std::endl; // C++11 adds: acos, asin, & atan
   std::cout << "std::tanh(c)            :: " <<  std::tanh(c)            << std::endl; // C++11 adds: acosh, asinh, & atanh
   std::cout << "std::sqrt(c)            :: " <<  std::sqrt(c)            << std::endl;
   std::cout << "(c*c)                   :: " <<  (c*c)                   << std::endl;
   std::cout << "std::abs(c)             :: " <<  std::abs(c)             << std::endl;
   std::cout << "std::pow(std::abs(c),2) :: " <<  std::pow(std::abs(c),2) << std::endl;
   std::cout << "(c*std::conj(c))        :: " <<  (c*std::conj(c))        << std::endl;
   std::cout << "std::real(c)            :: " <<  std::real(c)            << std::endl;
   std::cout << "std::imag(c)            :: " <<  std::imag(c)            << std::endl;

} /* end function main */
