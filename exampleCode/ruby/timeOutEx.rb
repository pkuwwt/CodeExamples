#!/usr/local/bin/ruby

##
# @file      timeOutEx.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Demonstrate the timeout module.@EOL
# @Keywords  ruby example timeout
# @Std       Ruby 1.8
#
#            The timeout module provides an easy way to abort
#            operations that may hang.  Normally one must wire up this
#            sort of thing by hand.  The typical technique involves
#            forking off a process to do the potentially dangerous
#            task while monitoring that task from the original
#            process.  With timeout, we just use the normal Ruby
#            begin/rescue/end clause with timeout().  Kool.

require 'timeout'

begin
  timeout(5) {
    sleep(10)
  }
  puts("The sleep worked..  Normally can't get here!!")
rescue Timeout::Error    
  puts("Timed out!")
end
