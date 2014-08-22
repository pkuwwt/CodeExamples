#!/usr/local/bin/ruby

##
# @file      webServerWEBrick02.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Simple web servlet with Ruby.@EOL
# @Keywords  ruby example web server webrick servlet
# @Std       Ruby 1.8
#
#            This little ruby program takes a port number, and starts
#            a web server.  The web server then provides up two
#            different services provided at URLS /foo and /.
#
#            Hit the server with:  http://LOCAL_IP_ADDRESS:PORT/foo
#             or
#            Hit the server with:  http://LOCAL_IP_ADDRESS:PORT/

require 'webrick'
include WEBrick

port = ARGV[0].to_i
print("Starting web server on port #{port}\n")
aServer = HTTPServer.new( :Port => port )

class FooServlet < HTTPServlet::AbstractServlet
  def do_GET(req, res)
    res['Content-Type'] = "text/html"
    res.body = "<html><body>Ruby Servlet-FOO!<br>" +
               "Parameters: #{req.query.keys.join(', ')}" +
               "</body></html>"
  end
end

class RootServlet  < HTTPServlet::AbstractServlet
  def do_GET(req, res)
    res['Content-Type'] = "text/html"
    res.body = "<html><body>" +
               "You should try /foo at the end of the URL<br>" +
               "Or even give it some parms: /foo?name=mitch" +
               "</body></html>"
  end
end

# Attach the servlets to the server..
aServer.mount("/", RootServlet)
aServer.mount("/foo", FooServlet)

# Set things to clean up .
trap("INT") { aServer.shutdown }

# Start the server.
aServer.start
