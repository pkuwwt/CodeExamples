#!/usr/local/bin/ruby
# -*- Mode:Ruby; Coding:utf-8; fill-column:132 -*-

####################################################################################################################################
# @file      /home/richmit/world/my_prog/learn/ruby/argParse.rb
# @author    Mitch Richling <http://www.mitchr.me>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @Revision  $Revision$ 
# @SCMdate   $Date$
# @brief     Using the 'optparse' module.@EOL
# @Keywords  ruby example command line arguments optparse
# @Std       Ruby 1.8
#
#            The optparse module provides a handy way to parse traditional command line arguments.  The magic of closures are
#            generally used as in this example to set variables based upon the argument parsing results.
#            

##----------------------------------------------------------------------------------------------------------------------------------

require 'optparse'

myOptionParser = OptionParser.new

# Must initialize required variables for options now, so the blocks we
# feed to myOptionParser will know about them.

doWeNeedHelp = false

# Set up calls for various arguments:
myOptionParser.on("-h", "--help", "-H") { print("Got help option\n"); doWeNeedHelp = true }
myOptionParser.on("--name aString")     { |str| print("Your name is: #{str}\n") }

# Now that the call backs are set, we parse the arguments
myOptionParser.parse(ARGV) rescue print("An Error On Argument Parse..\n")

if(doWeNeedHelp)
  print("We have a help option!!!\n")
end

print("Bye!\n")
