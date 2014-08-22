#!/usr/local/bin/ruby

##
# @file      fileIO.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Simple file I/O in ruby.@EOL
# @Keywords  example ruby file open IO
# @Std       Ruby 1.8


# Ruby has the lisp-like ability to pass an open file to a code block,
# and automatically close the file upon exit of the block.  Use a
# begin/rescue to catch I/O errors.  YAHOOO!

open("foo.txt", "w") do |file|
  file.puts("Hello")
  file.puts("World!")
end

puts("Read it one byte at a time:")
open("foo.txt", "r") do |file|
  file.each_byte do |byte|
    # putc knows about chars.  Like: print byte.chr
    putc byte
  end
end

puts("Read it one line at a time:")
open("foo.txt", "r") do |file|
  file.each_line do |line|
    puts(line)
  end
end

# One common way to do things is to slurp up the file all at once, and
# then manipulate it using Ruby's rich String methods.
puts("Read it all at one time into a string!")
open("foo.txt", "r") do |file|
  puts(file.read())
end

# Another common thing to do is slurp up the file all at once such
# that each line of the file is placed into an array.  Then we
# manipulate the array of lines (strings)
puts("Read it all at one time into an array!")
open("foo.txt", "r") do |file|
  lineArray = file.readlines()
  lineArray.each do |line|
    puts(line)
  end
end

# If all you want to do is slurp up a file into a string or into an
# array of lines, you can do it like this:
#
# aString = IO::read("fileName");
# anArray = IO::readlines("fileName");

# We have used a block to automatically close the file up till this
# point; however, more traditional (C/C++ style) methods may be
# used. When the "processing" block for the file is large or complex,
# this syntax can lead to cleaner code and finer error management.

# Sorta C/Perl-like syntax:
puts("Open it the old fashioned way, and dump it:")
if(file = open("foo.txt", "r"))
  puts(file.read())
  file.close
else
 # Error code here
end

# Sorta C++-like syntax:
puts("Get the file with C++-like syntax, and dump it:")
if(file = File.new("foo.txt", "r"))
  puts(file.read())
  file.close
else
 # Error code here
end

# Not only can we open the old fashioned way, but we can read the file
# the old fashioned way too -- i.e. read till we get an EOF.  Here we
# read it one line at a time.
puts("Open and read (one line at a time) the old fashioned way:")
if(file = open("foo.txt", "r"))
  until file.eof
	line = file.readline
    puts(line)
  end
  file.close
else
 # Error code here
end

# Finally, we take a nostalgic trip. Below we replicate one of the
# first file I/O programs found in K&R.  Here we open the file the
# C-like way, read it one byte at a time, and print out the bytes as
# we pull them from the file.  So quaint!
puts("Open and read (one byte at a time) the old fashioned way:")
if(file = open("foo.txt", "r"))
  until file.eof
	byte = file.readchar
    putc byte
  end
  file.close
else
 # Error code here
end
