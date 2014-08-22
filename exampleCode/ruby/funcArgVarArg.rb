#!/usr/local/bin/ruby

##
# @file      funcArgVarArg.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     How to support variable argument counts in Ruby functions.@EOL
# @Keywords  ruby example function vararg variable arguments
# @Std       Ruby 1.8

outSideFuncVar = 'dog'

def varArgFunc(arg1, *rest)
  printf("  arg1.class: %-20s arg1: %s\n", arg1.class, arg1.inspect)
  printf("  rest.class: %-20s rest: %s\n", rest.class, rest.inspect)
end

puts("varArgFunc(1)")
varArgFunc(1)

puts("varArgFunc(1, 2)")
varArgFunc(1, 2)

puts("varArgFunc(1, 2, 3)")
varArgFunc(1, 2, 3)

puts("varArgFunc(1, 'a', 3, [1,2,3])")
varArgFunc(1, 'a', 3, [1,2,3])
