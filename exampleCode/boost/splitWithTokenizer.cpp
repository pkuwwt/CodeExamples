// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
   @file      splitWithTokenizer.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 2014 by Mitch Richling.  All rights reserved.
   @brief     Split a string with the boost tokenizer.@EOL
   @Keywords  
   @Std       C++98

*/

//----------------------------------------------------------------------------------------------------------------------------------

/* ******** RCS FILE INFO **********
   $Id$

   $Log$
*/
#include <string>                  /* C++ strings             C++98/11 */
#include <vector>                  /* STL vector              C++98/11 */ 
#include <iostream>                /* C++ iostream            C++98/11 */

#include <boost/tokenizer.hpp>                       /* Tokenizer               Boost  */

std::vector<std::string> split(std::string aString, std::string feildSep);

//----------------------------------------------------------------------------------------------------------------------------------
int main(int argc, char* argv[]) {

  std::vector<std::string> feilds = split("aa,b,cccc,d,ee,f,g,,h", ",");

  for(std::vector<std::string>::iterator curFeild=feilds.begin(); curFeild!=feilds.end(); ++curFeild)
    std::cout << "\"" << *curFeild << "\"" <<  std::endl;
}


//----------------------------------------------------------------------------------------------------------------------------------
std::vector<std::string> split(std::string aString, std::string feildSep) {

  // Note: If you are always gonna use cstrings, then this double conversion dance is a bit inefficient...
  boost::char_separator<char> tokenSep(std::string(feildSep).c_str(), "", boost::keep_empty_tokens);

  // Build the tokenizer
  boost::tokenizer<boost::char_separator<char> > tok(aString, tokenSep);

  // A vector in which to place to put the results
  std::vector<std::string>* feilds = new std::vector<std::string>();

  // Walk acrossA vector in which to place to put the results
  for(boost::tokenizer<boost::char_separator<char> >::iterator curTok=tok.begin(); curTok!=tok.end(); ++curTok)
    feilds->push_back(*curTok);

  return *feilds;
}
