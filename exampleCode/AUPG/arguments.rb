#!/home/richmit/bin/verGo.sh ruby
# -*- Mode:Ruby; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      arguments.rb
# @author    Mitch Richling <http://www.mitchr.me>
# @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
# @brief     How to access command line arguments.@EOL@EOL
# @Keywords  Ruby command line arguments ARGV
# @Std       Ruby 1.8
#
#            This program works in any environment that lets ruby have command line arguments.
#            

##----------------------------------------------------------------------------------------------------------------------------------

printf("Program has %d argument%s (ARGV.size=%d)\n", ARGV.size, (ARGV.size == 1 ? "" : "s"), ARGV.size);

# Note that ARGV[0] is the first ARGUMENT, and not the name of the program.
ARGV.each_with_index do |arg,i|
  printf("Argument #%02d: %s\n", i+1, arg)
end
