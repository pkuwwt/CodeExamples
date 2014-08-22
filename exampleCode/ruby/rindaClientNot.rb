#!/usr/local/bin/ruby

##
# @file      rindaClientNot.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Rinda client program.@EOL
# @Keywords  ruby rinda distributed client notify
# @Std       Ruby 1.8
#
#            This is a client program for the rindaServer.rb script.
#            It demonstrates the Rinda notifications. For basic Rinda
#            functionality see the example rindaClientNot.rb.  Supply
#            the URI on the command line as the first and only
#            argument.

require 'rinda/tuplespace'

# Start drb and connect to server
DRb.start_service
ts=DRbObject.new(nil, ARGV.shift)

# notification types: 'write', 'take', & 'delete'.  Event type 'close'
# if entry expires..  Second arg is wild card for entries we want to
# watch.
observer = ts.notify('write', [2,nil])

# Most people fire off a thread to do the watching, but that is all we
# are gonna do in this program...
observer.each do |event, tuple|
  puts("OBJ: #{event.inspect} #{tuple.inspect}")
end

puts("Waiting for changes....")
puts("Press [enter] to exit...")
gets
