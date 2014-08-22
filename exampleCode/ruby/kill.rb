#!/usr/local/bin/ruby

##
# @file      kill.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Send/receive UNIX signals.@EOL
# @Keywords  ruby example signal kill
# @Std       Ruby 1.8
#
#            This script illustrates how to send signals.  It is
#            intended to send a SIGINT, SIGQUIT, and SIGKILL to a PID
#            specified on the command line.  That sequence of signals
#            is perfect input for the signals.rb example script.


if(ARGV.size < 1)
  puts("ERROR: Give me a process ID (PID)")
  exit
end

thePID = ARGV[0].to_i
puts("Sending signals to: #{thePID}")

puts("Sending SIGINT to #{thePID}")
Process.kill("SIGINT",  thePID);
sleep(1)

puts("Sending SIGQUIT to #{thePID}")
Process.kill("SIGQUIT", thePID);
sleep(1)

puts("Sending SIGKILL to #{thePID}")
Process.kill("SIGKILL", thePID);
sleep(1)
