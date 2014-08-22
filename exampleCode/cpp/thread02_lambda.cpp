// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      thread02_lambda.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
   @brief     Hello world for C++11 threads using a lambda.@EOL
   @Keywords  none
   @Std       C++11
***********************************************************************************************************************************/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#include <thread>
#include <iostream>

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int main(){

  // Construct thread (which will launch workerThread in a new thread)
  std::thread aThread([](){
      std::cout << "Hello from thread!" << std::endl;
    });

  // We "Join" with thread so that the main thread will wait for aThread to finish.
  aThread.join();

  std::cout << "DONE!" << std::endl;

  return 0;
} // end func main
