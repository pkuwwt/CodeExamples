#!/usr/local/bin/ruby

##
# @file      forkWait2.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Basic use of fork and wait2.@EOL
# @Keywords  ruby example fork 
# @Std       Ruby 1.8
#
#            This is essentially the same example as in forkWait.rb,
#            but we use the wait2 call instead of wait call.  I like
#            wait2 because it doesn't force me to use the global
#            variable $? to get child exit status.  OTOH, wait is more
#            POSIX-like, and that can make many programmers more
#            comfortable.  I might also select wait when don't care
#            about the child exit status.

aPID = Process.fork

if aPID
  # Parent
  sleep(1)
  STDERR.puts("I am parent(#{Process.pid}:#{Process.ppid})... Waiting for #{aPID}...\n")
  wait2out = Process.wait2(aPID)
  STDERR.puts("I am parent(#{Process.pid}:#{Process.ppid})... Child(#{wait2out[0]}) DONE.\n")
  STDERR.puts("Child status: #{wait2out[1].inspect}")
else    
  # Child
  STDERR.puts("I am child(#{Process.pid}:#{Process.ppid}).\n")
  sleep(3)
  exit(123)  # Exit with a number so we can see it in parent's output
end
