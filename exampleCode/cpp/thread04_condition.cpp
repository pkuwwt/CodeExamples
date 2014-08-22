// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      thread04_condition.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
   @brief     Demo C++11 condition variables.@EOL
   @Keywords  none
   @Std       C++11
***********************************************************************************************************************************/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#include <iostream>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <chrono>

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Potatoes have non-negative integer names.  We use -1 to mean the universe has run out of potatoes!  One common method used in C++
// to associate mutexes and condition_variables with the variables they are associated with is to put them all in a struct or class
// together.
//
// Style note: isHot must be set true when we put a new ID in a potato.  Setting them both external to the struct is a substandard
// OOP design -- we should have a method that sets them booth, and another to set a potato to cool.  That said, this program is
// about understanding thread locks and condition variables C++11, and I think it helps to see the components set explicitly near
// the thread lock manipulation code.  So, sorry for the substandard potato OOP.  At least I spelled potato correctly... :)

struct potato {
    bool isHot = false;   
    int ID;
    std::mutex mtx;
    std::condition_variable cv;
};

potato thePotato;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// This function will be called by a thread. It waits for a hot potato, and cools it. :)
void PotatoCooler() {
  std::unique_lock<std::mutex> thePotato_lock(thePotato.mtx ,std::defer_lock);
  while (true) {
    thePotato_lock.lock();
    std::cout << "work: waiting!" << std::endl;
    thePotato.cv.wait(thePotato_lock);
    if(thePotato.isHot) {
      std::cout << "work: pop " << thePotato.ID << std::endl;
      std::this_thread::sleep_for(std::chrono::milliseconds(5)); // Our Potatoes cool very quickly. :)
      thePotato.isHot = false;
      if(thePotato.ID == -1) {
        std::cout << "work: Bye Bye!" << std::endl;
        break;
      } // end if
    } // end if
    thePotato_lock.unlock();
  } // end while
  // We don't need to unlock the mutex as that will happen when it gets destroyed
} // end func PotatoCooler

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int main()  {

  // Construct and launch our thread
  std::thread thread(PotatoCooler);

  // Here we push a hot potato, and wait for PotatoCooler to cool it off..
  std::unique_lock<std::mutex> thePotato_lock(thePotato.mtx ,std::defer_lock);
  bool universeHasHotPotatoes = true;
  for(int potatoNumber=0; universeHasHotPotatoes; ) {
    thePotato_lock.lock();
    if(thePotato.isHot) {
      std::cout << "main: have new hot potato, but old one is still hot" << std::endl;
      thePotato_lock.unlock();
      thePotato.cv.notify_all();
      // Sleep is redundant with sleep at end of loop, but here so you can play with sleep location in this loop.
      std::this_thread::sleep_for(std::chrono::milliseconds(10));
    } else {
      if(potatoNumber<10) {
        std::cout << "main: push " << potatoNumber << std::endl;
        thePotato.ID = potatoNumber;
        thePotato.isHot = true;
        potatoNumber++;
        thePotato_lock.unlock();
        thePotato.cv.notify_all();
      } else {
        if(thePotato.isHot) {
          std::cout << "main: waiting for potato to cool" << std::endl;
        } else {
          universeHasHotPotatoes = false;
          thePotato_lock.unlock();
        } // end if/else
      } // end if/else
      // Wait for a bit before putting new work in thePotato. It is instructive to comment out this line, and watch what happens.
      std::this_thread::sleep_for(std::chrono::milliseconds(10));
    } // end if/else
  } // end for

  // Here we push the "quit working" work unit onto the thePotato queue.  We don't wait.
  thePotato_lock.lock();
  std::cout << "main: push " << -1 << std::endl;
  thePotato.ID = -1;
  thePotato.isHot = true;
  thePotato_lock.unlock();

  // Now we wait for the worker thread to process the "quit working" work unit.. :)
  bool potatoCoolerIsAlive = true;
  while(potatoCoolerIsAlive) {
    thePotato_lock.lock(); 
    std::cout << "main: HI " << std::endl;
    if(thePotato.isHot) {
      std::cout << "main: guard potato still hot" << std::endl;
      thePotato_lock.unlock();
      thePotato.cv.notify_all();
      std::this_thread::sleep_for(std::chrono::milliseconds(10));
    } else {
      std::cout << "main: guard potato is cool!" << std::endl;
      potatoCoolerIsAlive = false;
      thePotato_lock.unlock();
    } // end if/else
  } // end while

  std::cout << "main: joining " << std::endl;

  // Now we join just in case the worker thread has something to do before it can finish.
  thread.join();

  std::cout << "main: DONE!" << std::endl;
} // end func main
