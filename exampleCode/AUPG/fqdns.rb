#!/usr/local/bin/ruby
# -*- Mode:Ruby; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      fqdns.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
# @brief     How to get a host's FQDNS.@EOL
# @Keywords  FQDNS resolver hostname gethostbyname gethostbyaddr
# @Std       Ruby18
#
# Getting the FQDNS from a ruby program is not as simple as it should be for many UNIX platforms.  The difficulty stems from the
# fact that many UNIX platforms do not return FQDNS from a gethostbyname() call.  The related system call, gethostbyaddr() will
# always return the FQDNS, but you must know the host's IP address to use it.  The solution is to first get the IP address
# associated with a partial host name and then get the FQDNS name associated with it.

##----------------------------------------------------------------------------------------------------------------------------------

require 'resolv'
require "socket"

hostName = Socket.gethostname
hostIP = nil
Resolv.getaddresses(hostName).each do |a|
  if(a.to_s.match(/([0-9]+\.){3}[0-9]/)) then
    hostIP = a.to_s
    break
  end
end
hostName = Resolv::DNS.new.getname(hostIP)

puts("Name: #{hostName} Address 1: #{hostIP}")
