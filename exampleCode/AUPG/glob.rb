#!/usr/local/bin/ruby
# -*- Mode:Ruby; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      glob.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1995 by Mitch Richling.  All rights reserved.
# @brief     How to read directory entries@EOL
# @Keywords  perl glob
# @Std       Ruby 1.8.1
#
# This Ruby program is intended to illustrate how one reads the filenames in a directory with Perl's glob function.

##----------------------------------------------------------------------------------------------------------------------------------

dirToRead = ARGV[0]

# Read the directory entries.
dirEnts = Dir.glob("#{dirToRead}/*") + Dir.glob("#{dirToRead}/.*")

# Print out the info
dirEnts.each do |dirEnt| 
  puts("File: #{dirEnt}")
end
