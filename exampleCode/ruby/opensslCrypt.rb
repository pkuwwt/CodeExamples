#!/usr/local/bin/ruby

##
# @file      opensslCrypt.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Ruby OpenSSL example using Cypher.@EOL
# @Keywords  ruby openssl cypher des3
# @Std       Ruby 1.8

require 'openssl'

pText = "Hello, World!"
aKey  = 'hellohello'

puts("pText: #{pText.inspect}")

cipher = OpenSSL::Cipher::Cipher.new( "des3" )
cipher.encrypt(aKey)
cText = cipher.update(pText)  # repeatedly call this till no more data.
cText << cipher.final

cipher.decrypt(aKey)
pText = cipher.update(cText)
pText << cipher.final

puts("pText: #{pText.inspect}")
