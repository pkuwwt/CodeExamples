#!/usr/local/bin/ruby

##
# @file      forkPopenFork.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     How to fork and popen with child.@EOL
# @Keywords  ruby example fork popen
# @Std       Ruby 1.8
#
#            Ruby popen has a neat feature.  If the command is '-'
#            then another ruby interpreter is forked off and connected
#            to the current one via a pipe!  This is a very nice way to
#            fork off copies of the program and keep everyone in contact!

if(aPopenPipe = IO.popen('-', 'w+'))
  # This is the parent
  aPopenPipe.puts("PARENT SENT: Hello to my child")
  STDERR.puts("PARENT RECEIVED: '#{aPopenPipe.gets.chomp}'\n")
else
  # This is the child -- or the popen failed.
  STDOUT.puts("CHILD SEND: Hello to my parent!")
  STDERR.puts("CHILD RECEIVED: '#{STDIN.gets.chomp}'\n")
end
