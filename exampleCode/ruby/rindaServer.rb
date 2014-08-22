#!/usr/local/bin/ruby

##
# @file      rindaServer.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Rinda server program.@EOL
# @Keywords  ruby rinda distributed server
# @Std       Ruby 1.8
#
#            This is a server program for the client programs
#            rindaClientNot.rb and rindaClientSmpl.rb.

require 'rinda/tuplespace'

# Construct our tuple space 
ts = Rinda::TupleSpace.new

# Start a drb server attached to the tuple space A URI string may be
# given: "druby://foobar.dal.design.ti.com:1234" instead of nil.
DRb.start_service(nil, ts) 

puts("Server Running: #{DRb.uri}")

# Join with drb thread to avoid exit..
DRb.thread.join

puts("Server Dead")
