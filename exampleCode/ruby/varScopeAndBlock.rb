#!/usr/local/bin/ruby

##
# @file      varScopeAndBlock.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Local variables in blocks.@EOL
# @Keywords  ruby example local variables blocks
# @Std       Ruby 1.8

# The variable 'i' is first defined outside of the upto block, and so has scope
# of this entire file.  Thus the 'i' variable in the block to this same 'i' one.

i=10
1.upto(1) {
  i = 1
  puts("Hello World!\n")
}

puts("i: #{i}")

# The variable 'j' is "block local" as it is first defined in the upto
# block, and so it will NOT EXIST outside of that block.

1.upto(1) {
  j = 1
  puts("Hello World!\n")
}

puts("j: #{j}")  # This code is an ERROR -- i not in scope here!!!
