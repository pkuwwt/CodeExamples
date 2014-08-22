#!/usr/local/bin/ruby

##
# @file      funcBlockArg02.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Block arguments for ruby functions.@EOL
# @Keywords  ruby example function block arguments
# @Std       Ruby 1.8

def blockFun1(arg1, &aBlock)
  if aBlock # Note: Same as, but more readable, than 'block_given?'
    aBlock.call(arg1)
    "HAVE BLOCK"
  else
    "NO BLOCK"
  end
end

puts("Call with no block")
result = blockFun1('hello')
puts("  RESULT: #{result}")

puts("Call with block")
result = blockFun1('hello') {|x| puts("  IN BLOCK: #{x}")}
puts("  RESULT: #{result}")

puts("Call with block who ignores its argument")
result = blockFun1('hello') {puts("  IN BLOCK: DUNO ARG")}
puts("  RESULT: #{result}")
