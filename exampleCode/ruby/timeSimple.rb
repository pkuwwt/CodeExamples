#!/usr/local/bin/ruby

##
# @file      timeSimple.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Simple use of the Time class.@EOL
# @Keywords  Ruby example time date
# @Std       Ruby 1.8
#
#            Ruby has some sophisticated date and time classes (Date
#            for example), but the Time class is good enough for most
#            applications and feels like an old, familiar friend for
#            UNIX/POSIX programmers.  The Date class can do all sorts
#            of wonderful things with various calendars that the
#            simple Time class can never hope to do; however, the
#            price for this functionality is performance.  If Time has
#            what you need, then it will normally be an order of
#            magnitude faster than Date.

aTime = Time.now

puts("local & UTC Time")
aTime.localtime  # Side effect: Convert to local time zone
printf("Local Time:   %04d-%02d-%02d %02d:%02d:%02d %3s\n", 
       aTime.year, aTime.month, aTime.day,
       aTime.hour, aTime.min, aTime.sec, aTime.zone)

aTime.gmtime # Side effect: Convert to UTC (GMT) time zone
printf("  UTC Time:   %04d-%02d-%02d %02d:%02d:%02d %3s\n", 
       aTime.year, aTime.month, aTime.day,
       aTime.hour, aTime.min, aTime.sec, aTime.zone)

puts("Conversion To Strings:")
aTime.localtime
puts("  Local strftime : #{aTime.strftime('%Y-%m-%d %H:%M:%S %Z')}")
puts("  Local asctime  : #{aTime.asctime}")
puts("  Local to_s     : #{aTime.to_s}")
puts("  Local ctime    : #{aTime.ctime}")
puts("  Local inspect  : #{aTime.inspect}")
puts("  Local N/A      : #{aTime}")

aTime.gmtime
puts("  UTC   strftime : #{aTime.strftime('%Y-%m-%d %H:%M:%S %Z')}")
puts("  UTC   asctime  : #{aTime.asctime}")
puts("  UTC   to_s     : #{aTime.to_s}")
puts("  UTC   ctime    : #{aTime.ctime}")
puts("  UTC   inspect  : #{aTime.inspect}")
puts("  UTC   N/A      : #{aTime}")

puts("UNIX (POSIX) time integers")
puts("  From Int: #{Time.at(0)}")
puts("  To Int:   #{Time.at(0).tv_sec}")

puts("Construction from calendar components")
puts("  #{Time.local(2001, 11, 25, 1, 2, 3)}")
puts("  #{Time.gm(2001, 11, 25, 1, 2, 3)}")
