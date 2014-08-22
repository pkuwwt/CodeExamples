#!/usr/local/bin/ruby

##
# @file      webServer01s.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Simple SSL web server with Ruby.@EOL
# @Keywords  ruby example web server webrick openssl
# @Std       Ruby 1.8
#
#            This little ruby program works just like webServer01.rb,
#            but uses HTTPS instead of HTTP.  This requires OpenSSL.
#
#            Start up: 
#               ruby ./webServer01.rb 1234 /Users/richmit/world/my_prog/learn/ruby/
#
#            Try this URL: http://LOCAL_IP_ADDRESS:PORT/

require 'webrick'
require 'webrick/https'

port = ARGV[0].to_i
dir  = ARGV[1]

print("Starting web server on port #{port}\n")
print("Sharing directory #{dir}\n")

aServer = WEBrick::HTTPServer.new(
                                  :Port            => port,
                                  :DocumentRoot    => dir,
                                  :SSLEnable       => true,
                                  :SSLVerifyClient => ::OpenSSL::SSL::VERIFY_NONE,
                                  :SSLCertName => [ ["C","US"], ["O","127.0.0.1"], ["CN", "127.0.0.1"] ]
                                  )
trap("INT") { aServer.shutdown }
aServer.start



