#!/usr/local/bin/ruby

##
# @file      webServerMongrel02.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Simple web servlet with Mongrel in Ruby.@EOL
# @Keywords  ruby example web server mongrel servlet
# @Std       Ruby 1.8
#
#            This little ruby program takes a port number, and starts
#            a web server.  The web server then provides up two
#            different services provided at URLS /foo and /.
#
#            Hit the server with:  http://LOCAL_IP_ADDRESS:PORT/foo
#             or
#            Hit the server with:  http://LOCAL_IP_ADDRESS:PORT/

require 'rubygems'
require 'mongrel'

class FooServlet < Mongrel::HttpHandler
  def process(req, res)
    res.start(200) do |head, out|
      head['Content-Type'] = "text/html"
      out.write("<html><body>Ruby Servlet-FOO!<br>" +
                "Parameters: #{req.params['QUERY_STRING'].inspect}" +
                "</body></html>\n\n")
    end
  end
end

class RootServlet  < Mongrel::HttpHandler
  def process(req, res)
    res.start(200) do |head, out|
      head['Content-Type'] = "text/html"
      out.write("<html><body>" +
                "You should try /foo at the end of the URL<br>" +
                "Or even give it some parms: /foo?name=mitch" +
                "</body></html>\n\n")
    end
  end
end


port = ARGV[0].to_i
print("Starting web server on port #{port}\n")
mongrelSrv = Mongrel::HttpServer.new("127.0.0.1", port)

# Register the servlets
mongrelSrv.register("/foo", FooServlet.new)
mongrelSrv.register("/", RootServlet.new)

# Set things to clean up .
trap("INT") { aServer.shutdown }

# Start the server.
mongrelSrv.run.join
