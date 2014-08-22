#!/usr/local/bin/ruby

##
# @file      dirAndGlob.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Simple stuff with directories in Ruby.@EOL
# @Keywords  ruby example files glob
# @Std       Ruby 1.8

puts("Current working directory: #{Dir.pwd}\n")
puts("Current working directory: #{Dir.getwd}\n")

# Change directory with Dir.chdir(foo)
# Change root directory with: Dir.chroot(foo)

print("Some Ruby files in this directory ([de]*.rb): ")
Dir.glob('[de]*.rb').each do |d| 
  print("#{d} ")
end
puts()

# One can get a list of all files with: Dir.entries().  Note one can
# use Dir.pwd to get the CWD, but "." works as an argument to many
# methods in Dir.
puts("Found #{Dir.entries('.').length} files in CWD\n")

# One can traverse all entries with Dir.foreach() and/or Dir.each()
chars=0
Dir.foreach('.') do |d| 
  chars += d.length
end
puts("Found #{chars} characters in all file names in CWD")

# Ruby also provides the more traditional, readdir-like, APIs near and
# dear to the hearts of POSIX programmers.
numFiles=0
chars = 0
thePath = ''
Dir.open('.') do |aDir| 
  thePath = aDir.path   
  while fileName = aDir.read
    numFiles += 1
    chars += fileName.length
  end
end
puts("Found #{numFiles} filenames containing #{chars} characters in #{thePath} (CWD)")

# Of course, Ruby also provides a non-block version of open, and a
# close method to go along with it.  In addition, we also have the
# expected rewind() and seek() methods too.  Finally, we have some
# gravy methods: pos, pos=, tell.

# Finally, one can create directories with Dir.mkdir(), and delete
# directories with any of unlink(), delete(), or rmdir().
