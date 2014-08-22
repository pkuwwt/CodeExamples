// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      chrono01.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 2011 by Mitch Richling.  All rights reserved.
   @brief     Demo some of the features of the chrono header.@EOL
   @Keywords  time chrono date
   @Std       C++11
***********************************************************************************************************************************/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#include <iostream>
#include <chrono>
#include <thread>
 
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int main() {

  // The time_point class is for a real time clock.  You can use system_clock or steady_clock for a second based clock --
  // steady_clock always goes up even if a leap second or hour occurs.  Note the to_time_t return is the same as that used by the C
  // standard library time type found in ctime.
  std::chrono::time_point<std::chrono::system_clock> start(std::chrono::system_clock::now());
  std::cout << "Time is now: " << std::chrono::system_clock::to_time_t(start) << std::endl;

  // 'seconds' is a helper to create a durations (see also: nanoseconds, microseconds, milliseconds, seconds, minutes, and hours)
  std::this_thread::sleep_for(std::chrono::seconds(1));
  std::this_thread::sleep_for(std::chrono::milliseconds(300));
  
  std::chrono::time_point<std::chrono::system_clock> later(std::chrono::system_clock::now());
  std::cout << "Time is now: " << std::chrono::system_clock::to_time_t(later) << std::endl;

  // Durations can also be created from arithmetic operations on time_points.
  std::chrono::duration<double> timeDelta = later-start;

  // Normally .count comes out in seconds, but we can cast it to other units.
  std::cout << "The delta between times is: " << timeDelta.count()                                                        
            << " seconds"      << std::endl;
  std::cout << "The delta between times is: " << std::chrono::duration_cast<std::chrono::milliseconds>(timeDelta).count()
            << " milliseconds" << std::endl;

} /* end func main */
