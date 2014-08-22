#!/usr/local/bin/ruby

##
# @file      arg.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2005 by Mitch Richling.  All rights reserved.
# @brief     Accessing Ruby command line arguments.@EOL
# @Keywords  ruby example command line arguments
# @Std       Ruby 1.8
#
#            This example illustrates how to access command line
#            arguments via the ARGV array -- like @ARGV in Perl or
#            argv in C.  Ruby differs from many languages in that
#            ARGV[0] is the first proper argument to the script, and
#            NOT the command name (the script name).

print("Found #{ARGV.size} argument(s).\n\n")

print("ARGV: #{ARGV.inspect}\n\n")
