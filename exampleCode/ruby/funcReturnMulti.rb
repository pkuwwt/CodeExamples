#!/usr/local/bin/ruby

##
# @file      funcReturnMulti.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Returning multiple return values from Ruby functions.@EOL
# @Keywords  ruby example function return multiple
# @Std       Ruby 1.8

# Return can take multiple values.  When this happens, the return from
# the function is an array.  Note that a function may return different
# types depending upon the code path taken.  Thus this particular use
# of double return values as error code and result is not really
# necessary in ruby --- just return nil for an error and a float when
# the computation works.

def returnTwo(a, b)
  if (1.0*b) < 0.00001
    return 0.0, false
  else 
    return (1.0 * a / b), true
  end
end

puts("Return: #{returnTwo(1,0).inspect}")
puts("Return: #{returnTwo(1,2).inspect}")
