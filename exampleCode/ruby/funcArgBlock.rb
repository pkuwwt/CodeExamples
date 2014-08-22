#!/usr/local/bin/ruby

##
# @file      funcArgBlock.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Block arguments for ruby functions.@EOL
# @Keywords  ruby example function block arguments
# @Std       Ruby 1.8


# This function illustrates the block_given?/yield pattern.
def blockFun1(arg1)
  if block_given?
    yield(arg1)
    "HAVE BLOCK"
  else
    "NO BLOCK"
  end
end

puts("FUN1: Call with no block")
result = blockFun1('hello')
puts("  RESULT: #{result}")

puts("FUN1: Call with block")
result = blockFun1('hello') {|x| puts("  IN BLOCK: #{x}")}
puts("  RESULT: #{result}")

puts("FUN1: Call with block who ignores its argument")
result = blockFun1('hello') {puts("  IN BLOCK: DUNO ARG")}
puts("  RESULT: #{result}")

# This function illustrates the test-n-call pattern.  Many feel that
# it is more readable than the block_given?/yield pattern.
def blockFun2(arg1, &aBlock)
  if aBlock
    aBlock.call(arg1)
    "HAVE BLOCK"
  else
    "NO BLOCK"
  end
end

puts("FUN2: Call with no block")
result = blockFun2('hello')
puts("  RESULT: #{result}")

puts("FUN2: Call with block")
result = blockFun2('hello') {|x| puts("  IN BLOCK: #{x}")}
puts("  RESULT: #{result}")

puts("FUN2: Call with block who ignores its argument")
result = blockFun2('hello') {puts("  IN BLOCK: DUNO ARG")}
puts("  RESULT: #{result}")
