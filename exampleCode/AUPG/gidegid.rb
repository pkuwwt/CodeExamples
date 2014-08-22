#!/usr/local/bin/ruby
# -*- Mode:Ruby; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      gidegid.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     How to get the GID and EGID@EOL
# @Keywords  ruby GID EGID Process 
# @Std       ruby18
#
# This is an example program intended to illustrate how to query for user GID and effective user EGID.

##----------------------------------------------------------------------------------------------------------------------------------

# Figure out who the GID and EGID of the user is running this thing.

theGid  = Process.gid
theEgid = Process.egid
printf("gid: %d  egid: %d\n", theGid, theEgid);
