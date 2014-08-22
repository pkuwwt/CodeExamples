#!/usr/local/bin/ruby

##
# @file      rindaClientSmpl.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Rinda client program.@EOL
# @Keywords  ruby rinda distributed client
# @Std       Ruby 1.8
#
#            This is a client program for the rindaServer.rb script.
#            It demonstrates the basic functionality of Rinda except
#            for notification which is demonstrated in
#            rindaClientNot.rb.  Supply the URI on the command line as
#            the first and only argument.

require 'rinda/tuplespace'

# Start drb and connect to server
DRb.start_service
ts=DRbObject.new(nil, ARGV.shift)

# Add some tuples
ts.write([1, 'foo'])
ts.write([2, 'foo'])
ts.write([2, 'bar'])

# Get the list of tuples
puts("The list:")
allTuples = ts.read_all([nil,nil])
allTuples.each do |t|
  puts("#{t.inspect}")
end

#Get the first matching tuple
t = ts.read([2, nil])
puts("From Read #{t.inspect}")

# Delete the first '2' record.. only get one!!
t = ts.take([2, nil])
puts("We ganked: #{t.inspect}")

# Get the list again..
puts("The list:")
allTuples = ts.read_all([nil,nil])
allTuples.each do |t|
  puts("#{t.inspect}")
end

puts("Client Done")
