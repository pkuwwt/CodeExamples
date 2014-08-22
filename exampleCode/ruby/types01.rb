#!/usr/local/bin/ruby

##
# @file      types01.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Simple type concepts in Ruby.@EOL
# @Keywords  ruby example types
# @Std       Ruby 1.8

a = 1
b = "hello: "

puts(a.class())
puts(b.class())

# ERROR: a+b
# Convert a into string, and then add:
c = b + a.to_s

puts(c)
