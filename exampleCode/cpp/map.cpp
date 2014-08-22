// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      map.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 2000 by Mitch Richling.  All rights reserved.
   @brief     Example of how to use MAPs in the STL specified by C++ 98 .@EOL
   @Keywords  Standard C++ I/O character string iostream
   @Std       C++98
***********************************************************************************************************************************/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#include <map>                     /* STL map                 C++98/11 */
#include <string>                  /* C++ strings             C++98/11 */
#include <iostream>                /* C++ iostream            C++98/11 */

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int main() {

  std::map<std::string, std::string> myMap;

  myMap["foo"] = "first value";
  myMap["bar"] = "second value";

  // We can extract a value with the [] operator
  std::cout << "Find 'foo' with [] operator: map[\"foo\"]=" << myMap["foo"] << std::endl;

  // One must be careful with [], because if what you are looking for is NOT in the map, it will be added (The key will be what you
  // were looking for, and the value will be the default construction of the data value type.  We use count() to figure out if "zod"
  // is a key in the map.  For a map, count() will be 0 or 1 as keys must be unique in a map.  This is a typical idiom.
  std::cout << "zod is " << (myMap.count("zod")?"":"NOT ") << "in the map" << std::endl;  
  myMap["zod"];
  std::cout << "zod is " << (myMap.count("zod")?"":"NOT ") << "in the map" << std::endl;  

  // We can traverse the whole thing with an iterator.  The local variable in the for loop is a typical idiom -- just like ints in
  // an array traversal.  Note we must use the stuff from pair<> as that is what you get from the iterators in a map.
  std::cout << "All the elements of the map:" << std::endl;
  for(std::map<std::string, std::string>::iterator myMapIter = myMap.begin(); myMapIter != myMap.end(); ++myMapIter)
    std::cout << "  \"" << myMapIter->first << "\"  = " << myMapIter->second << std::endl;

  // How to look for something.
  std::map<std::string, std::string>::iterator myMapIter;

  // std::map's fast find should be used (not <algorithm>'s find).
  std::cout << "Looking for FOO... ";
  myMapIter = myMap.find("FOO");
  if(myMapIter != myMap.end())
    std::cout << "Found it: " << myMapIter->second << std::endl;
  else
    std::cout << "Didn't find it" << std::endl;

  // Just searching again to show the other case
  std::cout << "Looking for foo... ";
  myMapIter = myMap.find("foo");
  if(myMapIter != myMap.end())
    std::cout << "Found it: " << myMapIter->second << std::endl;
  else
    std::cout << "Didn't find it" << std::endl;

} // end func main

