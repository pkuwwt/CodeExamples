#!/usr/local/bin/ruby

##
# @file      mpmfs.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Test server for HTTP GET and HTTP POST.@EOL
# @Keywords  ruby web webrick get post test server
# @Std       Ruby 1.8
#
#            This script is intended to help test web clients by
#            providing a simple HTTP GET and HTTP POST test at:
#                http://127.0.0.1:8080/test
#                https://127.0.0.1:8080/test

require 'webrick'
require 'webrick/https'
require 'cgi'

include WEBrick

class ServiceTesterService < WEBrick::HTTPServlet::AbstractServlet
  # POST
  def do_POST(req, resp)
    resp.body = do_X(req, resp, 'POST')
    resp['Content-Type'] = 'text/html'
  end
  # GET
  def do_GET(req, resp)
    resp.body = do_X(req, resp, 'GET')
    resp['Content-Type'] = 'text/html'
  end
  # This really performs all the work.
  def do_X(req, resp, rtype)
    if(req.query) then
      thep = ''
      req.query.each do |key, val|
        thep += "KEY: #{CGI::escapeHTML(key.inspect)} == VAL: #{CGI::escapeHTML(val.inspect)}\n"
      end
      return '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN"><HTML><HEAD><TITLE>TEST' +
             "</TITLE></HEAD><BODY><H1>#{rtype} TEST</H1><PRE>#{thep}</PRE><br><br><br><hr></BODY></HTML>";
    else
      return '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN"><HTML><HEAD><TITLE>TEST ERROR' +
             '</TITLE></HEAD><BODY><H1>TEST ERROR</H1><br><br><br><hr></BODY></HTML>';
    end
  end
end

aServer1 = WEBrick::HTTPServer.new(:Port            => 8181, 
                                   :BindAddress     => '127.0.0.1',
                                   :SSLEnable       => true,
                                   :SSLVerifyClient => ::OpenSSL::SSL::VERIFY_NONE,
                                   :SSLCertName     => [ ["C","US"], ["O","127.0.0.1"], ["CN", "127.0.0.1"] ]
                                   )
aServer2 = HTTPServer.new(:Port => 8080, :BindAddress => '127.0.0.1')
aServer2.mount("/test", ServiceTesterService)
aServer1.mount("/test", ServiceTesterService)

['HUP', 'QUIT', 'INT', 'TERM', 'USR1', 'USR2'].each do |sig|
  trap(sig) { aServer1.shutdown; aServer2.shutdown }
end

# Start the servers up
Thread.new() { aServer1.start }
Thread.new() { aServer2.start }
# And wait for them to end..
Thread.list.each {|t| puts(t); t.join() unless (t == Thread.current) }
