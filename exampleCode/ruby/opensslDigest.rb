#!/usr/local/bin/ruby

##
# @file      opensslDigest.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Ruby OpenSSL example using Digest.@EOL
# @Keywords  example sha1 md5 ruby openssl digest
# @Std       Ruby 1.8
#
#            The 'digest' module is the most commonly used one for
#            simple message digests, and is pure Ruby -- no openssl
#            library required for operation.  Still, nice to know how
#            to make use of the openssl functionality too.

require 'openssl'     # For OpenSSL::Digest::MD5
require 'digest/md5'  # For Digest::MD5

pText = "Hello, World!"
puts("pText: #{pText.inspect}")

dText = OpenSSL::Digest::MD5.hexdigest(pText)  # See: SHA1 also
puts("dText (openssl): #{dText.inspect}")

dText = Digest::MD5.hexdigest(pText)
puts("dText (digest):  #{dText.inspect}")
