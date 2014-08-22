#!/usr/local/bin/ruby
# -*- Mode:Ruby; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      group.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
# @brief     How to get group data@EOL
# @Keywords  ruby group unix grgrent
# @Std       ruby 1.8.1
#
# Cats the group table.  Works with the defined name service because it uses the getgrent function which is implemented using the
# getgrent system call.

##----------------------------------------------------------------------------------------------------------------------------------

require 'etc' 

# A "Ruby-like" construct is provided by the group iterator:
Etc.group do |g|
  puts("#{g.name}:#{g.passwd}:#{g.gid}:#{g.mem.join(' ')}")
end
