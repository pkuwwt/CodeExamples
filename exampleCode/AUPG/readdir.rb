#!/usr/local/bin/ruby
# -*- Mode:Ruby; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      readdir.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1994 by Mitch Richling.  All rights reserved.
# @brief     UNIX directory access.@EOL
# @Keywords  UNIX filename directory UFS Ruby opendir readdir
# @Std       Ruby
#
# This Ruby program is intended to illustrate how one reads UNIX directories(file names in a subdirectory); however, we
# illustrate the Ruby-esq approach over the more POSIX-like readdir interface.

##----------------------------------------------------------------------------------------------------------------------------------

dirToRead = ARGV.shift;

Dir.foreach(dirToRead) do |dirEnt|
  printf("File: %s\n", dirEnt);
end
