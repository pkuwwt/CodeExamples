#!/usr/local/bin/ruby

##
# @file      if01.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     if-then construct in Ruby.@EOL
# @Keywords  ruby example
# @Std       Ruby 1.8

if 1 > 2 
  puts('TRUE')
elsif 3 > 4
  puts('TRUE2')
else
  puts('BOTH FALSE')
end

if (true) then puts('TRUE') end

puts('TRUE') if (1==1)

puts('FALSE') unless (1!=1)
