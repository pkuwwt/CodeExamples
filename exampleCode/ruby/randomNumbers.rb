#!/usr/local/bin/ruby

##
# @file      randomNumbers.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Generate random numbers with Ruby.@EOL
# @Keywords  ruby example random mersenne twister
# @Std       Ruby 1.8
#
#            The stock random number generator in Ruby is actually
#            quite good mathematically speaking.  It is even good
#            enough for semi-serious simulation work.  Unlike the
#            linear congruence generators typically found in many
#            languages, Ruby uses a version of the Mersenne Twister
#            with a period of around 2**19937-1.
#
#            WARNING: Before you go off and start writing simulation
#            code with Ruby's default generator, please check that
#            your version of Ruby still uses this generator or a
#            better one -- things change in the software world.


# First we will seed the generator to a nice, simple number.  Use an
# integer, but don't use 0.
Kernel.srand(1)

puts("Some random numbers in [0, 1)")
1.upto(5) {
  print("#{Kernel.rand()} ")
}
puts

puts("Now some random numbers in [0, 100.to_i.abs)")
1.upto(5) {
  print("#{Kernel.rand(100)} ")
}
puts
