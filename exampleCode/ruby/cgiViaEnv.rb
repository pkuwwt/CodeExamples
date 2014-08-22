#!/usr/local/bin/ruby

##
# @file      cgi02.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Simple CGI example with using Ruby's cgi module.@EOL
# @Keywords  ruby example cgi
# @Std       Ruby 1.8
#
#            A no-module CGI example.
#
#            Try: http://ADDRESS/cgi-bin/cgi02.rb?name=Mitch%20Richling&age=0&age=1

require 'cgi'

print("Content-type: text/html\n\n")
print("<html><body>")

print("<p>We received the following parameters: ")
print(CGI.escapeHTML(ENV['QUERY_STRING'].inspect))
print("<br><br>\n")

ENV['QUERY_STRING'].split(/&/).each do |kvp|
  k, v = kvp.split('=')
  print("&nbsp;&nbsp;&nbsp;&nbsp;*&nbsp;")
  print(CGI.escapeHTML(k))
  print("&nbsp;")
  print(CGI.escapeHTML("=>"))
  print("&nbsp;")
  print(CGI.escapeHTML(v))
  print("<br>\n")
end

print("</body></html>\n")
