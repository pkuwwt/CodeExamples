#!/usr/local/bin/ruby

##
# @file      webServer01.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Simple web server with Ruby.@EOL
# @Keywords  ruby example web server webrick
# @Std       Ruby 1.8
#
#            This little ruby program takes a port number and a
#            directory name.  It then starts up a web server on the
#            given port number and serves up the given directory.
#
#            Any file with a .cgi will be evaluated like a CGI script
#            would -- so this little bit of code is a nice way to test
#            CGI scripts without starting up a traditional HTTP
#            server!  How cool is that!
#
#            No error checking in this script -- beware!
#
#            Start up: 
#               ruby ./webServer01.rb 1234 /Users/richmit/world/my_prog/learn/ruby/
#
#            Try this URL: http://LOCAL_IP_ADDRESS:PORT/

require 'webrick'
include WEBrick

port = ARGV[0].to_i
dir  = ARGV[1]

print("Starting web server on port #{port}\n")
print("Sharing directory #{dir}\n")

aServer = HTTPServer.new( :Port => port, :DocumentRoot => dir)
trap("INT") { aServer.shutdown }
aServer.start
