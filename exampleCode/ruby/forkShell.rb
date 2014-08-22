#!/usr/local/bin/ruby

##
# @file      forkShell.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     How to use perl-like back quote shell escape.@EOL
# @Keywords  ruby example fork shell back quote
# @Std       Ruby 1.8
#
#            Like Perl and sh, backtics produce a string containing
#            the STDOUT of the given command.

# lsOut will contain a string with the output of the ls command.
lsOut = `ls -l /`

# The most common idiom is to process the string one line at a time:
print("==========================\n")
print("Command output:\n")
lsOut.each_line { |line|
        # Note, newline at end of line is preserved..
        print(line)
}

# Another common idiom is to transform the string into an array of
# lines, and then process the array in some way:
print("==========================\n")
print("Command output:\n")
lsOut.split("\n").each_with_index { |line, idx|
       printf("%05d:  %s\n", idx, line)
}
