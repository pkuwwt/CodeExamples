#!/home/richmit/bin/verGo.sh ruby
# -*- Mode:Ruby; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      utime.rb
# @author    Mitch Richling <http://www.mitchr.me>
# @Copyright Copyright 2011 by Mitch Richling.  All rights reserved.
# @brief     How to change mtime and atime on a file.@EOL
# @Keywords  timestamp touch file atime mtime ruby
# @Std       Ruby1.8.1

##----------------------------------------------------------------------------------------------------------------------------------

if(ARGV.size != 1) then
  puts("ERROR: A file name is required as the single argument.")
  exit(1)
end

atime = 0
mtime = 0

File.utime(atime, mtime, ARGV[0])
