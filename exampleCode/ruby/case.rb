#!/usr/local/bin/ruby

##
# @file      case.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Demonstrate the case statement. @EOL
# @Keywords  ruby case
# @Std       Ruby 1.8

def caseExample(anInteger)
  case anInteger
    when 0 
      "CASE(0)"
    when 1 
      "CASE(1)"
    when 2..5 
      "CASE(2..5)"
    when 6..8 
      "CASE(6..8)"
    else 
      "CASE(else)"
  end
end

0.upto(15) {|i| printf("%3d: %s\n", i, caseExample(i)) }

# One can put a 'then in the case.
case 2
  when 0..1 then puts("CASE(0..1)")
  when 2..3 then puts("CASE(2..3)")
end

# Alternate forms:

case 1
 when 0    then "CASE(0)"
 when 1    then "CASE(1)"
 when 2..5 then "CASE(2..5)"
 when 6..8 then "CASE(6..8)"
 else           "CASE(else)"
end

case 1
 when 0    : "CASE(0)"
 when 1    : "CASE(1)"
 when 2..5 : "CASE(2..5)"
 when 6..8 : "CASE(6..8)"
 else        "CASE(else)"
end
