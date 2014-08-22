#!/usr/local/bin/ruby

##
# @file      termSize.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2008 by Mitch Richling.  All rights reserved.
# @brief     Query the terminal size via ioctl.@EOL
# @Keywords  ioctl terminal size TIOCGWINSZ
# @Std       Ruby 1.8
#
#            This is a rather non-portable technique as the TIOCGWINSZ
#            constant is not standard -- too bad that Ruby doesn't
#            extract the constant values from the system header files
#            at install time....
#            

TIOCGWINSZ = 0x40087468   # This is the IOCTL number for my Intel Mac (OS 10.5)

ioctlRet = [ 0, 0, 0, 0 ].pack("SSSS")
if ($stdin.ioctl(TIOCGWINSZ, ioctlRet) >= 0) then
  rows, cols, xPixCnt, yPixCnt = ioctlRet.unpack("SSSS")
  printf("Rows: %d\n", rows)
  printf("Cols: %d\n", cols)
  printf("X-px: %d\n", xPixCnt)
  printf("Y-px: %d\n", yPixCnt)
else
  puts("ERROR: Something went wrong!")
end
