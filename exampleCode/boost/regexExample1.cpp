// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
   @file      regexExample1.cpp
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 2004 by Mitch Richling.  All rights reserved.
   @brief     Boost regular expression example@EOL
   @Keywords  none
   @Std       C++98 Boost

   Simple, but complete, example showing the most typical use of the Boost regular expression library.
              
*/

//----------------------------------------------------------------------------------------------------------------------------------

#include <iostream>                /* C++ iostream            C++98/11 */
#include <string>                  /* C++ strings             C++98/11 */
#include <boost/regex.hpp>                           /* RegEx                   Boost  */

int main() {

  /* Our strings and string forms for the regualr expressions. */
  char const* s1  = "hello, world";
  char const* s2  = "hello, world!  hello, bob!";
  char const* es1 = "hello";
  char const* es2 = ".*hello.*";
  char const* es3 = ".*(hello).*(world).*";

  /* Build (compile) our regular expressions */
  boost::regex e1(es1);
  boost::regex e2(es2);
  boost::regex e3(es3);

  /* A place to put matchs (and sub-matchs) */
  boost::cmatch what;     

  /* The thing to note is that the re must match the ENTIRE string. */
  std::cout << "regex_match(s1, e1): " << boost::regex_match(s1, e1) << std::endl;
  std::cout << "regex_match(s1, e2): " << boost::regex_match(s1, e2) << std::endl;

  /* We can find sub-expressions with regex_match too */
  std::cout << "regex_match(s1, what, e3): " << boost::regex_match(s1, what, e3) << std::endl;
  std::cout << "what[0] = " << std::string(what[0].first, what[0].second) << std::endl;
  std::cout << "what[1] = " << std::string(what[1].first, what[1].second) << std::endl;
  std::cout << "what[2] = " << std::string(what[2].first, what[2].second) << std::endl;

  /* You can use regex_format to build perl-like subistitution rules. */
  std::cout << "Perl like expand ('$1-$2') = " <<  regex_format(what, "$1-$2") << std::endl;

  /* regex_match can be used much like the s// operator in perl */
  std::cout << "perlish s2=~s/e1/HELLO/g: " <<   regex_merge(std::string(s2), e1, "HELLO") << std::endl;

  /* Note that with regex_search, we don't have to match the whole line. */
  std::cout << "regex_search(s1, what, e1): " << boost::regex_search(s1, what, e1) << std::endl;
  std::cout << "what[0] = " << std::string(what[0].first, what[0].second) << std::endl;

}
