#!/usr/local/bin/ruby

##
# @file      forkBlock.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     How to use fork with a block in Ruby.@EOL
# @Keywords  ruby example fork block
# @Std       Ruby 1.8
#
#            Ruby's fork can take a block.  If given a block, then the
#            block is executed in the child.  The parent continues on
#            it's marry little way.  This is a great way to "kick off
#            some code" in a different process, and continue working
#            on something else.

aPID = Kernel.fork do 
  STDERR.puts("I am child.\n")
  sleep(3)
end  

if aPID
  # Parent
  sleep(1)
  STDERR.puts("I am parent... Waiting...\n")
  Process.wait(aPID)
  STDERR.puts("I am parent... Child DONE. Bye!\n")
else
  # The child will never run outside of the block, so
  # this will only be reached if the fork failed!
  STDERR.puts("I am parent... Fork FAILED! Bye!\n")
end
