#!/usr/local/bin/ruby

##
# @file      forkWait.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Basic use of fork and wait.@EOL
# @Keywords  ruby example fork 
# @Std       Ruby 1.8
#
#            The most basic use of the fork & wait is illustrated.
#            See forkWait2 for how to use wait2.  See the other fork*
#            examples for other ways to fork off processes.  Note that
#            fork is also exposed in Kernel.fork -- one frequently
#            sees it called as "Kernel.fork" or just "fork".
#
#            We don't illustrate waitall, but it is very handy..

aPID = Process.fork

if aPID
  # Parent
  sleep(1)
  STDERR.puts("I am parent(#{Process.pid}:#{Process.ppid})... Waiting for #{aPID}...\n")
  donePID = Process.wait(aPID)
  STDERR.puts("I am parent(#{Process.pid}:#{Process.ppid})... Child(#{donePID}) DONE.\n")
  STDERR.puts("Child status: #{$?.inspect}")
else    
  # Child
  STDERR.puts("I am child(#{Process.pid}:#{Process.ppid}).\n")
  sleep(3)
  exit(123)  # Exit with a number so we can see it in parent's output
end
