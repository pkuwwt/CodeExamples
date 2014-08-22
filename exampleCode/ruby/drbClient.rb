#!/usr/local/bin/ruby

##
# @file      drbClient.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Simple DRb client.@EOL
# @Keywords  ruby example drb corba client
# @Std       Ruby 1.8
#
#            Like Corba, DRb provides a distributed object model for
#            Ruby.  Also like Corba, it can be incredibly complex to
#            get everything correct.  Unlike Corba, DRb makes simple
#            stuff easy.  This little example is a simple client for
#            the random number server implemented in drbServer.rb.

require 'drb'

# Our server URI
ourURI="druby://127.0.0.1:1234"

# Start a local DRbServer for callbacks.
DRb.start_service

# Create our proxy object
localProxyRandObject = DRbObject.new_with_uri(ourURI)

# Call methods in our proxy object
puts("Seed with 1..")
localProxyRandObject.remote_srand(1)
puts(localProxyRandObject.remote_rand())
puts(localProxyRandObject.remote_rand())
puts(localProxyRandObject.remote_rand())
puts("Re-seed with 1..")
localProxyRandObject.remote_srand(1)
puts(localProxyRandObject.remote_rand())
