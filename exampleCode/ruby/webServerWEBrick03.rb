#!/usr/local/bin/ruby

##
# @file      webServerWEBrick03.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Simple web server with Ruby.@EOL
# @Keywords  ruby example web server webrick
# @Std       Ruby 1.8
#
#            This little ruby program demonstrates how to use the mount_proc method
#
#            Try this URL: http://localhost:8080/simpleProc

require 'webrick'
include WEBrick

aServer = HTTPServer.new( :Port => 8080, :DocumentRoot => dir, :BindAddress => 'localhost'a\)
trap("INT") { aServer.shutdown }
trap("TERM") { aServer.shutdown }

# We can add ultra-simple "servlets" with just a tiny bit of code.
# The CGI stuff will be in "@query_string".
aServer.mount_proc('/simpleProc') {|req, resp| 
  resp.body = "The full req: #{req.inspect}"
} 

aServer.start
