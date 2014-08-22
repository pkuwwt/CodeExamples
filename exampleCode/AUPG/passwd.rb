#!/usr/local/bin/ruby 
# -*- Mode:Ruby; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      passwd.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Password database access in Ruby.@EOL
# @Keywords  UNIX password passwd database getpwent
# @Std       Ruby
#
# Cats the password data base to standard out.  Works with whatever name service is configured because it makes use of the
# getpwent() function, and that function is implemented with the system call getpwent().

##----------------------------------------------------------------------------------------------------------------------------------

require 'etc' 

i=0;
Etc.passwd do |p|
  i += 1
  printf("User #%05d: %s:%s:%d:%d:%s:%s:%s\n", i,
         p.name, p.passwd, p.uid, p.gid, p.gecos, p.dir, p.shell)
end
