#!/usr/local/bin/ruby

##
# @file      funcArgArrayExpand.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     How to expand arrays into multiple function arguments.@EOL
# @Keywords  ruby example function array expand arguments
# @Std       Ruby 1.8
#
#            Frequently one must construct the argument list for a
#            function call, and it is handy to place such argument
#            lists in an array.  Once constructed, one must figure out
#            how to call the function.  Luckily this is easy in Ruby.

sysArg = [ 'ls', '-l', '-d', '/' ]

# the entire argument list expanded from one array.
system(*sysArg)

# The final arguments may be expanded from an array too -- this only
# works with the array as the LAST argument.
system('echo', *sysArg)
