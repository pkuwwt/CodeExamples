// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
   @file      tokenExample.cpp
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 2004 by Mitch Richling.  All rights reserved.
   @brief     Boost tokenizer example@EOL
   @Keywords  none
   @Std       C++98 Boost

   Simple, but complete, example showing the most typical use of the Boost tokenizer template.  This example uses only delimiter
   separator, and shows how one might parse a UNIX password record.
              
*/

//----------------------------------------------------------------------------------------------------------------------------------


#include <iostream>                /* C++ iostream            C++98/11 */
#include <boost/tokenizer.hpp>                       /* Tokenizer               Boost  */
#include <string>                  /* C++ strings             C++98/11 */

//----------------------------------------------------------------------------------------------------------------------------------
int main(){
  std::string passwdString = "root:*:0:0:System Administrator:/var/root:/bin/sh::";
  // It's a good idea to typedef this as we use it twice even in this little example
  typedef boost::tokenizer<boost::char_separator<char> > passwdTokenizer;

  boost::char_separator<char> tokenSep(":", "", boost::keep_empty_tokens);
  passwdTokenizer tok(passwdString, tokenSep);
  for(passwdTokenizer::iterator curTok=tok.begin(); curTok!=tok.end(); ++curTok)
          std::cout << "'" << *curTok << "'" << std::endl;
}
 
