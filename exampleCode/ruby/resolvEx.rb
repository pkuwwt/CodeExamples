#!/usr/local/bin/ruby

##
# @file      resolvEx.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     How to use current host name and DNS info with resolv.@EOL
# @Keywords  ruby example resolver hostname host name
# @Std       Ruby 1.8
#
#            Note that all methods (each_address, each_name,
#            getaddress, getaddresses, getname, getnames, &
#            getresources) are all available for Hosts and DNS -- we
#            just illustrate a few for each class.

require 'resolv'
require "socket"

puts("OUR HOST INFO VIA RESOLVER")
ourHostName = Socket.gethostname
ourMainIP   = Resolv.getaddress(ourHostName)
puts("  #{Resolv.getaddress(ourHostName).inspect    }")
puts("  #{Resolv.getaddresses(ourHostName).inspect  }")
puts("  #{Resolv.getname(ourMainIP).inspect         }")
puts("  #{Resolv.getnames(ourMainIP).inspect        }"        )

puts("OUR HOST INFO VIA HOST FILE")
hostf = Resolv::Hosts.new
hostf.each_address(ourHostName) do |a| 
  puts("  #{a.inspect}")
end
hostf.each_name(ourMainIP) do |a| 
  puts("  #{a.inspect}")
end

puts("USE DNS TO GET A RECORDS (www.google.com)")
dns = Resolv::DNS.new
dns.getresources("www.google.com", Resolv::DNS::Resource::IN::A).each do |r| 
  puts("  #{r.address}")
end
