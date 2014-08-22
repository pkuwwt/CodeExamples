#!/usr/local/bin/ruby

##
# @file      drbServer.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Simple DRb server.@EOL
# @Keywords  ruby example drb corba server
# @Std       Ruby 1.8
#
#            Like Corba, DRb provides a distributed object model for
#            Ruby.  Also like Corba, it can be incredibly complex to
#            get everything correct.  Unlike Corba, DRb makes simple
#            stuff easy.  This little example starts up a simple
#            random number server on the local host.  Use the
#            drbClient.rb example to connect to this server.

require 'drb'

# Our server URI
ourURI="druby://127.0.0.1:1234"

class RandomNumberServer
  def remote_rand
    return rand()
  end
  def remote_srand(seed)
    return srand(seed)
  end
end

# This object will be exposed via DRb
drbExposedObject=RandomNumberServer.new

$SAFE = 1  

# Start up the DRb service
DRb.start_service(ourURI, drbExposedObject)
puts("DRb server started..")

# Hang around till the DRb thread goes away
DRb.thread.join

puts("DRb server shutdown.. Bye!")

