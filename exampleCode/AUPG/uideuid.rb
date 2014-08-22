#!/usr/local/bin/ruby
# -*- Mode:Ruby; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      uideuid.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
# @brief     UID and EUID in ruby.@EOL
# @Keywords  UID EUID Perl
# @Std       ruby1.8.1 POSIX, BSD4.3, SVID3
#
# This is an example program intended to illustrate how to query for user ID and effective user ID.

##----------------------------------------------------------------------------------------------------------------------------------

# Figure out who the user is running this thing.
theUid = Process.uid
theEuid = Process.euid
puts("uid: #{theUid}  euid: #{theEuid}")
