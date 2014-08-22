#!/usr/local/bin/ruby

##
# @file      fileNames.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Working with file names in Ruby.@EOL
# @Keywords  ruby examples file names basename dirname extname identical
# @Std       Ruby 1.8
#
#            The File class provides several handy functions for
#            dealing with file names.  Some of them are simple string
#            manipulation tools while others answer tough filesystem
#            questions.
#
#            Note this example is a bit UNIX-ey in that we assume that
#            the path delimiter is the forward slash (/) character...


# Join strings together to form paths.
puts("PATH: #{File.join('foo', 'bar', 'foobar')}")
puts("PATH: #{File.join('/foo', 'bar', 'foobar')}")

puts("Parts of '/foo/bar/foobar.rb'")
pathPart, filepart = File.split("/foo/bar/foobar")
puts("  PATH PART: #{pathPart}")
puts("  FILE PART: #{filepart}")

puts("Parts of '/foo/bar/foobar.rb'")
puts("  BASENAME (with ext): #{File.basename('/foo/bar/foobar.rb')}")
puts("  BASENAME (no ext):   #{File.basename('/foo/bar/foobar.rb', '.rb')}")
puts("  DIRNAME:             #{File.dirname('/foo/bar/foobar.rb')}")
puts("  EXTNAME:             #{File.extname('/foo/bar/foobar.rb')}")

# The previous tools are simple string manipulation tools; however,
# the next methods are more sophisticated and deal with real some
# tricky filesystem issues.

# Expand a path with dots or links:
puts("Fully expanded CWD: '#{File.expand_path('.')}'")

# Do the same but specify the current working directory:
puts("Fully expanded . from /tmp/: '#{File.expand_path('..', '/tmp/')}'")

# Now a magical tool.  File.identical? determines if two files point to
# the same file (are linked together for example).  On my system,
# /usr/local/bin/ruby is linked to /opt/local/bin/ruby, but
# /usr/bin/ruby is the one provided by the OS vendor (Apple).
puts("/usr/local/bin/ruby == /usr/bin/ruby: #{File.identical?('/usr/local/bin/ruby', '/usr/bin/ruby')}")
puts("/usr/local/bin/ruby == /opt/local/bin/ruby: #{File.identical?('/usr/local/bin/ruby', '/opt/local/bin/ruby')}")
