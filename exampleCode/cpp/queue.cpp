// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      queue.cpp
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 2014 by Mitch Richling.  All rights reserved.
   @brief     Example of how to use QUEUEs in the STL specified by C++ 98 .@EOL
   @Keywords  Standard C++ I/O character string iostream STL queue template
   @Std       C++98

   This program is an example of how use the queue template in the STL of C++ 98.  The queue template an adapter that will transform
   any conainer supporting front(), back(), push_back(), and pop_front() into a FIFO stack supporting front(), back(), push(), and
   pop().  Note that queues support empty() and size() too.              
***********************************************************************************************************************************/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#include <queue>                   /* STL queue               C++98/11 */
#include <iostream>                /* C++ iostream            C++98/11 */

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int main() {

  std::queue<int> myQueue;

  for(int i=0;i<10;i++) {
	myQueue.push(i);
  } // end for

  std::cout << "First: " << myQueue.front() << std::endl;
  std::cout << "Last:  " << myQueue.back()  << std::endl;
  std::cout << "Size:  " << myQueue.size()  << std::endl;

  // We can traverse, and empty, the whole thing like this:
  std::cout << "All the elements of the queue:" << std::endl;
  for(int i=0; myQueue.size()>0; i++) {
	std::cout << i << ": " << myQueue.front() << std::endl;
	myQueue.pop();
  } // end for

} // end func main
