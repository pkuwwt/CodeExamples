#!/usr/local/bin/ruby

##
# @file      webServerWEBrick04.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Example illustrating code generation and closures@EOL
# @Keywords  ruby examples webrick closure
# @Std       Ruby 1.8
#
#            One of Ruby's strengths is the ability to generate code
#            on the fly.  This example generates procedures to provide
#            servlets for several simple web pages.

require 'webrick'
include WEBrick

require 'cgi'

# Create the server object
STDOUT.puts("INFO(webShareDir): Starting up web server now...")
aServer = HTTPServer.new(:Port => 8080, :BindAddress => 'localhost')

# Build procs for each provided command
[ [ 'hostname'   , 'hostname'                        ],
  [ 'uname'      , 'uname -a'                        ],
  [ 'uptime'     , 'uptime'                          ],
  [ 'df'         , 'df -k'                           ],
  [ 'mount'      , 'mount'                           ],
  [ 'domainname' , 'domainname'                      ],
  [ 'who'        , 'who'                             ],
  [ 'last'       , 'last | head -40'                 ]].each do |commandURL, command|
  aServer.mount_proc('/' + commandURL) {|req, resp| 
    resp.body =
      '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN">              ' +
      '<HTML>                                                               ' +
      '  <HEAD><TITLE>' + "SYSTEM INFO: #{commandURL}" + '</TITLE>          ' +
      '  </HEAD>                                                            ' +
      '  <BODY>                                                             ' +
      '    <H1>' + "SYSTEM INFO: #{command}" + '</H1>                       ' +
      '    <PRE>' + "\n" + CGI::escapeHTML(`#{command}`) + "\n" + '</PRE>   ' +                       
      '  </BODY>                                                            ' +
      '</HTML>                                                              '    
    resp['Content-Type'] = 'text/html'
  }
end

# Trap server shutdown signals
trap("INT") { aServer.shutdown }
trap("TERM") { aServer.shutdown }

# Start the server up
aServer.start
