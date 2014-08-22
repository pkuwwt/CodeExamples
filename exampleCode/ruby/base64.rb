#!/usr/local/bin/ruby

##
# @file      base64.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Demonstrate the base64 module@EOL
# @Keywords  base64 uuencode uudecode ruby
# @Std       Ruby 1.8

require 'base64'

pText = 'Hello, World!'
puts("pText: '#{pText.inspect}")

cText = Base64.encode64(pText)
puts("cText: '#{cText.inspect}")

pText = Base64.decode64(cText)
puts("pText: '#{pText.inspect}")

