// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
   @file      threadCond.cc
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
   @brief     boost threads example@EOL
   @Keywords  boost thread example
   @Std       C++98

   Just about minimal example of the boost thread library.  Only the thread and mutex classes are demonstrated.
              
*/

//----------------------------------------------------------------------------------------------------------------------------------


#include <boost/thread.hpp>                          /* Threads                 Boost  */

#include <iostream>                /* C++ iostream            C++98/11 */
#include <unistd.h>                /* UNIX std stf            POSIX    */

boost::mutex console;
boost::mutex condMutex;
boost::condition_variable aCond;

//----------------------------------------------------------------------------------------------------------------------------------
struct threadS {
  int id;
  threadS(int tid) : id(tid) { }
  void operator()() {
    for(int i=0;i<10;i++) {
      boost::mutex::scoped_lock aLock(condMutex);
      aCond.wait(aLock);
      {
        boost::mutex::scoped_lock aLock(console);
        std::cout << "thread: " << id << " " << i << std::endl;
      }
    }
  }

};

//----------------------------------------------------------------------------------------------------------------------------------
int main(int argc, char* argv[]) {
  std::cout << "Hello..." << std::endl;
  threadS one(1);
  threadS two(2);
  boost::thread oneThread(one);
  boost::thread twoThread(two);

  // Normally one would loop till the threads were gone....
  for(int i=0;i<20;i++) {
    {
      boost::mutex::scoped_lock aLock(console);
      std::cout << "main: Release The Hounds...." << std::endl;
    }
    aCond.notify_all();
    sleep(1);
  }

  oneThread.join();
  twoThread.join();
  std::cout << "bye.." << std::endl;   
}
