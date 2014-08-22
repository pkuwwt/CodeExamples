#!/usr/local/bin/ruby

##
# @file      forkSystem.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     How to use the system() call in ruby.@EOL
# @Keywords  ruby example fork system
# @Std       Ruby 1.8
#
#            Ruby has a system() function very similar to the one in
#            Perl. As with Perl, the command output (STDOUT and
#            STDERR) escapes directly to the same STDOUT and STDERR
#            used by the Ruby script.  The good news is that all the
#            same shell redirection tricks available in Perl work with
#            Ruby too.

# true or nil (if the system call failed)..
result = system("ls")
if( !(result))
  print("Something went wrong!  Return code: #{$?}\n")
end

puts("The result: #{result.inspect}")
