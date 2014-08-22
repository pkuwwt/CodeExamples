#!/usr/local/bin/ruby

##
# @file      forkDetach.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Basic use of fork.@EOL
# @Keywords  ruby example fork detach
# @Std       Ruby 1.8
#
#            This example is very similar to forkWait.rb and
#            forkWait2.rb, except this program abandons the child with
#            Process.detach.  In this way, the child will never
#            become a zombie, but we don't have to call wait().  

aPID = Process.fork

if aPID
  # Parent
  sleep(1)
  STDERR.puts("I am parent(#{Process.pid}:#{Process.ppid})... Child is #{aPID}...\n")
  donePID = Process.detach(aPID)
  STDERR.puts("I am parent(#{Process.pid}:#{Process.ppid})...  I have abandoned my child.\n")
  sleep(10)
  STDERR.puts("I am parent(#{Process.pid}:#{Process.ppid})...  Bye!\n")
else    
  # Child
  STDERR.puts("I am child(#{Process.pid}:#{Process.ppid}).\n")
  sleep(3)
  STDERR.puts("I am child(#{Process.pid}:#{Process.ppid}).  I am dieing.. BYE!\n")
  exit(123)
end
