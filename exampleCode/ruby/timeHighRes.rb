#!/usr/local/bin/ruby

##
# @file      timeHighRes.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     High resolution timers in Ruby.@EOL
# @Keywords  ruby example time microsecond
# @Std       Ruby 1.8
#
#            Ruby provides reasonable access to micro-second counters
#            and sleeps. Micro-second resolution time data may be
#            accessed via the .usec or .tv_usec members of Time while
#            one second time may be accessed via .to_i or .tv_sec
#            members.  For sub-second resolution sleeps, simply
#            provide the sleep() method a floating point number.
#
#            Not all operating environments provide sub-second
#            resolution for time, so be careful.
#

aTime = Time.now
puts("usec=#{aTime.usec} sec=#{aTime.to_i}")
sleep(1)
aTime = Time.now
puts("usec=#{aTime.usec} sec=#{aTime.to_i}")
sleep(0.001)
aTime = Time.now
puts("usec=#{aTime.usec} sec=#{aTime.to_i}")
aTime = Time.now
puts("usec=#{aTime.usec} sec=#{aTime.to_i}")
