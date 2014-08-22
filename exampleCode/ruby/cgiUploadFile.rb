#!/usr/local/bin/ruby

##
# @file      cgi03.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @Revision  $Revision$ 
# @SCMdate   $Date$
# @brief     Dealing with 'file' parameters in CGI.@EOL
# @Keywords  
# @Std       Ruby 1.8
#
#            How to handle file uploads to a web server with Ruby CGI
#            scripts.

require 'cgi'

print("Content-type: text/html\n\n")
print("<html><body>")

myCGI = CGI.new

if (myCGI.params['uploadfilename'][0].original_filename &&
    myCGI.params['uploadfilename'][0].original_filename.size > 0) then

  print("<h1>File Name:" + 
        CGI.escapeHTML(myCGI.params['uploadfilename'][0].original_filename.inspect) +
        "</h1>")

  begin
    print("<h1>First 10 Bytes:</h1>")
    a = myCGI.params['uploadfilename'][0].read()
    print(CGI.escapeHTML(a[0, ( a.size < 10 ? a.size : 10 )]))
  rescue
    print("<h1>ERROR: Could Not Read File</h1>")
  end

else

  print("<h1>ERROR: Invalid or Missing File Name</h1>")

end
 
print("</body></html>\n")

