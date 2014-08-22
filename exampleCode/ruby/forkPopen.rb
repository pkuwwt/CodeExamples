#!/usr/local/bin/ruby

##
# @file      forkPopen.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     How to use ruby popen.@EOL
# @Keywords  ruby example fork popen
# @Std       Ruby 1.8
#
#            Ruby supports C's popen -- Much like Perl's open('foo |')
#            construct.  I/O can go both ways.  It is just STDOUT and
#            STDIN that get mapped.

# One can use a rather perl/C-like syntax.
aPopenPipe = IO.popen('ls -l | head -5', 'r')
aPopenPipe.each_line do |line|
  print("OUT1: #{line.chomp}\n")
end
aPopenPipe.close;

# The obvious, more ruby-like syntax also works.
IO.popen('ls -l | head -5', 'r') do |aPopenPipe|
  aPopenPipe.each_line do |line|
    print("OUT2: #{line.chomp}\n")
  end
end

# Much like in perl, using '|' in a "file" will open a pipe
# too. Unlike Perl, the read/write nature of the pipe is not
# determined by the location of the pipe character.  Rather, in Ruby,
# the the second argument to the open call determines read/write and
# the pipe character is ALWAYS the first character of the first
# argument (the "file name").
Kernel.open('| ls -l | head -5', 'r') do |anInFile|
  anInFile.each_line do |line|
    print("OUT3: #{line.chomp}\n")
  end
end
