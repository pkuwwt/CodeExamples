// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
   @file      threadBind.cc
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
   @brief     boost threads example@EOL
   @Keywords  boost thread bind example
   @Std       C++98

   Just about minimal example of the boost thread library using a typical bind class paradigm -- a paradigm that should make
   LISP/Ruby programmers very comfortable.  Only the thread and mutex classes are demonstrated.
              
*/

//----------------------------------------------------------------------------------------------------------------------------------


#include <boost/thread.hpp>                          /* Threads                 Boost  */
#include <boost/bind.hpp>                            /* Functors                Boost  */
#include <iostream>                /* C++ iostream            C++98/11 */
#include <unistd.h>                /* UNIX std stf            POSIX    */

boost::mutex console;

//----------------------------------------------------------------------------------------------------------------------------------
void threadS(int id) {
  for(int i=0;i<10;i++) {
    { // This block exists to destory aLock when not needed.  See other lock methods too: try_mutex & timed_mutex
      boost::mutex::scoped_lock aLock(console);
      std::cout << "thread: " << id << " " << i << std::endl;
    }
    sleep(1);
  }
}

//----------------------------------------------------------------------------------------------------------------------------------
int main(int argc, char* argv[]) {
  std::cout << "Hello..." << std::endl;
  boost::thread oneThread(boost::bind(&threadS, 1));
  boost::thread twoThread(boost::bind(&threadS, 2));
  oneThread.join();
  twoThread.join();
  std::cout << "bye.." << std::endl;   
}

