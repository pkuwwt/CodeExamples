// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      regexExample1.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 2013 by Mitch Richling.  All rights reserved.
   @brief     C++11 regex example.@EOL
   @Keywords  regex C++
   @Std       C++11

   Simple, but complete, example showing the most typical use of the C++ regular expression library.
***********************************************************************************************************************************/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#include <iostream>                /* C++ iostream            C++98/11 */
#include <string>                  /* C++ strings             C++98/11 */
#include <iterator>                /* STL Iterators           C++98/11 */
#include <regex>                   /* C++ RegEx               C++11    */

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int main() {

  // Our strings and string forms for the regualr expressions.
  char const* s1  = "hello, world";
  char const* s2  = "hello, world!  hello, bob!";
  char const* es1 = "hello";
  char const* es2 = ".*hello.*";
  char const* es3 = ".*(hello).*(world).*";

  // Build (compile) our regular expressions 
  std::regex e1(es1);
  std::regex e2(es2);
  std::regex e3(es3);

  // A place to put matchs (and sub-matchs) 
  std::cmatch what;     

  // The thing to note is that the re must match the ENTIRE string.
  std::cout << "regex_match(s1, e1): " << std::regex_match(s1, e1) << std::endl;
  std::cout << "regex_match(s1, e2): " << std::regex_match(s1, e2) << std::endl;

  // Note that with regex_search, we don't have to match the whole line.
  std::cout << "regex_search(s1, e1): " << std::regex_search(s1, e1) << std::endl;
  std::cout << "regex_search(s1, e2): " << std::regex_search(s1, e2) << std::endl;

  // We can find sub-expressions with regex_match (or regex_search)
  std::cout << "regex_match(s1, what, e3): " << std::regex_match(s1, what, e3) << std::endl;
  std::cout << "Num substrings: " << what.size() << std::endl;
  std::cout << "what[0] = " << std::string(what[0].first, what[0].second) << std::endl;
  // std::cout << "what[1] = " << std::string(what[1].first, what[1].second) << std::endl;
  // std::cout << "what[2] = " << std::string(what[2].first, what[2].second) << std::endl;

  // regex_replace can be used much like the s// operator in perl
  std::cout << "perlish s2=~s/e1/HELLO/g: " << std::regex_replace(std::string(s2), e1, std::string("HELLO")) << std::endl;

} // end func main




