#!/usr/local/bin/ruby

##
# @file      caseInt.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     The case statement with (class) types. @EOL
# @Keywords  ruby case class ===
# @Std       Ruby 1.8
#
#            Ruby uses the === operator in the case statement, which
#            makes the case statement a useful tool when categorizing
#            objects into groups based upon class/sub-class.


def caseExample(anObject)
  case anObject
    when Array
      puts("Array")
    when Array
      puts("Hash")
    when Fixnum
      puts("Fixnum")
    when Numeric
      puts("Number")   # A Float is a Number, 
    else 
      puts("ELSE")
  end
end

caseExample(Array.new)
caseExample(1)
caseExample(1.2)
caseExample(Hash.new)

