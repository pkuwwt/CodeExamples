// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      readFileLines.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 1997,1998,2014 by Mitch Richling.  All rights reserved.
   @brief     Read a file line by line and stuff each line into a vector.@EOL
   @Keywords  none
   @Std       C++98
***********************************************************************************************************************************/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#include <iostream>                /* C++ iostream            C++98/11 */
#include <vector>                  /* STL vector              C++98/11 */ 
#include <fstream>                 /* C++ fstream             C++98/11 */

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int main (int argc, char *argv[]) {

  if(argc>1) {
    std::ifstream ifile(argv[1]);
    // If we just want to exit on I/O errors, then the following line will do it.
    //ifile.exceptions ( std::ifstream::failbit | std::ifstream::badbit );
    if (ifile.good() && ifile.is_open()) {
      std::vector<std::string> fileData;
      for(std::string line; getline(ifile, line);) {
        fileData.push_back(line);
        std::cout << "L: '" << line << "'" << std::endl;
      } // end for
      if (!ifile.eof()) {
        std::cout << "ERROR: Something bad happend durring file read"  << std::endl;
      } // end if   
      // Do something with fileData HERE
    } else {
      std::cout << "ERROR: Unable to open file (" << argv[1] << ")" << std::endl;
    } // end if/else
  } else {
    std::cout << "ERROR: Missing argument!" << std::endl;
  } // end if/else

} // end func main
