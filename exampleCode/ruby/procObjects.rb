#!/usr/local/bin/ruby

##
# @file      procObjects.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Procedure Objects.@EOL
# @Keywords  ruby procedure objects signal handler trap
# @Std       Ruby 1.8
#
#            Ruby uses closures everyplace.  The procedure object is
#            just another way to pass around code.


# This is a procedure object
helloObject = proc {
  puts("Hello!")
}

# You can run the code in a procedure object via the call method
helloObject.call

# One place you might use a procedure object is for a signal handler
trap "SIGINT", helloObject

# Now we just wait for the user to hit Ctrl-C...
puts("Hit Ctrl-C (^C).. You have 10 seconds..")
puts("LOOP 4")
10.downto(1) {|i| puts(i); sleep(1); }

puts("Bye!")
