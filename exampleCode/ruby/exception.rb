#!/usr/local/bin/ruby

##
# @file      exception.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Exception handling basics in ruby.@EOL
# @Keywords  ruby example begin rescue exception handling 
# @Std       Ruby 1.8

generate_NoMethodError = true

begin
  puts("Begin code running")
  if(generate_NoMethodError)
    nil[1]
  else
    1/0
  end
  puts("Begin code finishing")
rescue NoMethodError
  puts("Caught a missing method exception")
rescue # the default rescue
  puts("A bugaboo happened")
ensure # always run this code
  puts("Ensure code always runs")
end


## Re-raise current exception:
#raise 
#
## Raise RuntimeError exception with message "Error Message" 
#raise "Error Message" 
#
## Raise AnException exception with message "Error Message" 
#raise AnException, "Error Message" 
