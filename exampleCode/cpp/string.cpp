// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      string.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 2000 by Mitch Richling.  All rights reserved.
   @brief     Example of how to use strings in C++ 98.@EOL
   @Keywords  Standard C++ STL string
   @Std       C++98

   While string is NOT part of the STL as a container, it is very container-like.
***********************************************************************************************************************************/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#include <string>                  /* C++ strings             C++98/11 */
#include <iostream>                /* C++ iostream            C++98/11 */

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int main() {
  // strings start life empty with the default constructor
  std::string s1, s2, s3;

  // strings may be constructed from c-strings like so
  std::string s4("hello");
  std::string s5("12345678901234567890");

  // You can assign a c-string to a string
  s1 = "HAL";

  // operator+ is overridden and understands strings AND c-strings so that the two can be freely intermixed:
  s2 = s4 + " my name is " + s1;

  // You can also use += like this:
  s2 += "!";
  
  // Or you can append like this.
  s2.append("!");

  // Be careful and don't try to add two c-strings.  The following would not generally work: s3 = "I " + "like " + s1; Do
  // something like this to get around the problem:
  s3 = std::string("I ") + "like " + s1;

  // cout knows how to print both c-strings and strings;
  std::cout << "Strings at this point:" << std::endl;
  std::cout << "  s1='" << s1 <<  "'" << std::endl;
  std::cout << "  s2='" << s2 <<  "'" << std::endl;
  std::cout << "  s3='" << s3 <<  "'" << std::endl;
  std::cout << "  s4='" << s4 <<  "'" << std::endl;
  std::cout << "  s5='" << s5 <<  "'" << std::endl;

  // The length of a string may be found with length():
  std::cout << "The length of s5=" << s5.length() << std::endl;

  // Substrings are easy:
  std::cout << "The s5.substr(3,7):='" << s5.substr(3, 7) << "'" << std::endl;

  // Strings may be indexed like arrays.
  std::cout << "char 3 of s5 (s5[2])='" << s5[2] << "'" << std::endl;

  // Assignment works too so long as the char type has =:
  s5[2] = 'X';
  std::cout << "After s5[2]='X', s5='" << s5 << "'" << std::endl;

  // You can erase and collapse substrings:
  std::cout << "The s5.erase(3,7):='" << s5.erase(3, 7) << "' (now only " << 
	s5.length() << " chars)" << std::endl;

  // You can insert strings, substrings, c-strings, and characters.
  std::cout << "After s5.insert(5, 'YYYY'):='"    << s5.insert(5, "YYYY")    << "' (now at " << s5.length() << " chars)" << std::endl;
  std::cout << "After s5.insert(7, 'Z'):='"       << s5.insert(7, "Y")       << "' (now at " << s5.length() << " chars)" << std::endl;
  std::cout << "After s5.insert(10, s4):='"       << s5.insert(10, s4)       << "' (now at " << s5.length() << " chars)" << std::endl;
  std::cout << "After s5.insert(10, s4, 2, 2):='" << s5.insert(10, s4, 2, 2) << "' (now at " << s5.length() << " chars)" << std::endl;

  // If your char is equality comparable, then so are strings.  The == operator also knows what to do with c-strings.
  if(s1 == "HAL")
	std::cout << "TRUE:  s1=='HAL'" << std::endl;
  if( !(s1 == "hAL"))
	std::cout << "FALSE: s1=='hAL'" << std::endl;

  // If your char is LessThan comparable, then so are strings.  The operators deal with c-strings. (all work: < > <= >=)
  if(s1 < "hal")
	std::cout << "TRUE:  s1<'hal'" << std::endl;

  // It is easy to search for substrings within strings.  
  std::cout << "s3.find('HAL')=" << s3.find("HAL") << std::endl;
  std::cout << "s3.find('hAL')=" << s3.find("hAL") << std::endl;
} // end func main
