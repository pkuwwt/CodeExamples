#!/usr/local/bin/ruby

##
# @file      funcReturn.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Return values for ruby functions.@EOL
# @Keywords  ruby example function return
# @Std       Ruby 1.8

# All the sumIt* functions do the same thing:

def sumIt1(a, b)
  result = a + b;
  return result
end

def sumIt2(a, b)
  result = a + b;
end

def sumIt3(a, b)
  a + b;
end

# Start the main program --- note functions have to be defined before
# they get used.

puts(sumIt1(1,2))
puts(sumIt2(1,2))
puts(sumIt3(1,2))
