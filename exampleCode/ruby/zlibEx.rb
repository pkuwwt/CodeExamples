#!/usr/local/bin/ruby

##
# @file      zlibEx.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Example showing typical zlib usage in Ruby.@EOL
# @Keywords  ruby zlib libz gzip gunzip
# @Std       Ruby 1.8

require 'zlib'

# One can use the GzipWriter and GzipReader much like File.open for working with
# compressed files.  This is similar to the gz* functions in libz.

Zlib::GzipWriter.open("testGzipFile.gz") do |gzfile|
  gzfile.write(`ls -l /tmp/ | head -5`)
end

Zlib::GzipReader.open("testGzipFile.gz") do |gzfile|
  gzfile.each_line do |line| 
    puts(line)
  end
end

# GzipReader and GzipWriter may be constructed from an existing stream object.
# While zlib's gzdopen function provides similar functionality in C, Ruby's OOP
# goodness really shines through here by making this functionality quite natural.

File.open("testGzipFile.gz") do |file|
  gzfile = Zlib::GzipReader.new(file)
  gzfile.each_line do |line| 
    puts(line)
  end
end
