// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
   @file      boostRandEx.cpp
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 2004 by Mitch Richling.  All rights reserved.
   @brief     Boost random number generator example@EOL
   @Keywords  none
   @Std       C++98 Boost

   Simple, but complete, example showing the most typical use of the Boost random number generator API.  From a mathematical
   perspective, several of the base generators are quite good.
              
*/

//----------------------------------------------------------------------------------------------------------------------------------

#include <iostream>                /* C++ iostream            C++98/11 */
#include <fstream>                 /* C++ fstream             C++98/11 */

#include <boost/random/linear_congruential.hpp>      /* PRNGs                   Boost  */
#include <boost/random/uniform_int.hpp>              /* PRNGs                   Boost  */
#include <boost/random/uniform_real.hpp>             /* PRNGs                   Boost  */
#include <boost/random/variate_generator.hpp>        /* PRNGs                   Boost  */

int main() {
  // Define a base random number generator and initialize it with a seed.
  // Always make sure seed argument unsigned for mt19937
  // Typical choices:  boost::mt19937, boost::ecuyer1988, boost::minstd_rand
  boost::minstd_rand baseGen(1234u);

  // Define distribution U[0,1) [double values]
  boost::uniform_real<> uniDblUnit(0,1);

  // Define a random variate generator using our base generator and distribution
  boost::variate_generator<boost::minstd_rand&, boost::uniform_real<> > uniDblGen(baseGen, uniDblUnit);

  // Get some random numbers:
  std::cout << "10 samples of a uniform distribution in [0..1):\n";
  for(int i = 0; i < 10; i++)
    std::cout << uniDblGen() << '\n';

  // Define integer, discrete uniform distribution [1, 16]
  boost::uniform_int<> uniInt16(1,16);

  // Define a random variate generator using our base generator and distribution
  boost::variate_generator<boost::minstd_rand&, boost::uniform_int<> > uniIntGen(baseGen, uniInt16);

  // Get some random numbers:
  std::cout << "10 samples of a discrete, integer uniform distribution on [1..16]:\n";
  for(int i = 0; i < 10; i++)
    std::cout << uniIntGen() << '\n';
  
  // Keep silly compilers quiet
  return 0;
}
