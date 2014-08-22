#!/usr/local/bin/ruby

##
# @file      funcLocalVar.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Function local variable concepts in Ruby. @EOL
# @Keywords  ruby example function local variable
# @Std       Ruby 1.8

outSideFuncVar = 'dog'

def exampleFunc
  funcLocal = 'hello'
  outSideFuncVar = 'world'
  puts("IN FUNC:  #{funcLocal} #{outSideFuncVar}!")
  return funcLocal
end

puts("OUT FUNC: #{exampleFunc} #{outSideFuncVar}!")

#puts(funcLocal)  ## ERROR: funcLocal is local to exampleFunc
