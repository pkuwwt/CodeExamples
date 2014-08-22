#!/usr/local/bin/ruby

##
# @file      cgiParm.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Simple CGI example with using Ruby's cgi module.@EOL
# @Keywords  ruby example cgi
# @Std       Ruby 1.8
#
#            In this example we use the cgi module.  Only simple
#            parameter extraction is demonstrated along with the
#            escapeHTML method.
#
#            Try: http://ADDRESS/cgi-bin/cgi02.rb?name=Mitch%20Richling&age=0&age=1

require 'cgi'

print("Content-type: text/html\n\n")
print("<html><body>")

myCGI = CGI.new

print("<p>Hello<br>\n")

print("<p>We received the following parameters: ")
print(CGI.escapeHTML(myCGI.params.keys.inspect))
print("\n")

print("<p>They have the following values:<br>\n")
myCGI.params.each do |param, values|
  print("&nbsp;&nbsp;&nbsp;&nbsp;*&nbsp;")
  print(CGI.escapeHTML(param.inspect))
  print("&nbsp;")
  print(CGI.escapeHTML("=>"))
  print("&nbsp;")
  print(CGI.escapeHTML(values.inspect))
  print("<br>\n")
end

if(myCGI['name'])
  print("<p>We have a NAME: #{myCGI['name'].inspect}")
  print("<p>We have a NAME: #{myCGI.params['name'][0].inspect}")
end

print("</body></html>\n")
