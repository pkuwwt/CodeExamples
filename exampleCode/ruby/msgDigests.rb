#!/usr/local/bin/ruby

##
# @file      msgDigests.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Message digests with Ruby.@EOL
# @Keywords  message digest md5 sha1
# @Std       Ruby 1.8

require 'digest/md5' 
require 'digest/sha1' 

aString = "Now is the time for all good men to come to the aid of their country"

puts(" MD5: #{Digest::MD5.hexdigest(aString)}\n")
puts("SHA1: #{Digest::SHA1.hexdigest(aString)}\n")
