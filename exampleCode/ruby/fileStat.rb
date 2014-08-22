#!/usr/local/bin/ruby

##
# @file      fileStat.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     File Stat with Ruby.@EOL
# @Keywords  ruby example file lstat
# @Std       Ruby 1.8
#
#             FileTest provides a great many tests, but File::Stat is
#             the place to look for file meta data like time stamps.
#             You can also do file tests with stat, and if you have
#             already stat'ed a file, it is faster to test the
#             existing stat structure than to use FileTest.
#
#             Some of the stuff in the stat class:
#               File Times
#                  stat.atime
#                  stat.ctime
#                  stat.mtime
#               Device holding file
#                  stat.blksize => native file system block size
#                  stat.blockdev? => block device?
#                  stat.dev => device (Major and minor parts: stat.dev_major & stat.dev_minor)
#               Size related info
#                  stat.blocks => number of blocks
#                  stat.size => size of stat in bytes.
#                  stat.zero? => zero length file?
#               File type info
#                  stat.chardev?
#                  stat.directory?
#                  stat.file?
#                  stat.ftype => file type
#                  stat.pipe?
#                  stat.socket?
#                  stat.symlink?
#               Owner info
#                  stat.gid => numeric group id of the owner of stat.
#                  stat.ino => inode number for stat.
#                  stat.uid => numeric user id of the owner of stat.
#               File mode info
#                  stat.executable?
#                  stat.executable_real?
#                  stat.grpowned?
#                  stat.mode => integer permission bits
#                  stat.owned?
#                  stat.readable?
#                  stat.readable_real?
#                  stat.setgid?
#                  stat.setuid?
#                  stat.sticky?
#                  stat.writable?
#                  stat.writable_real?
#               Misc
#                  stat.nlink => number of hard links to stat.


# We stat a file (this source code file)..
begin  
  statData = File::Stat.new('fileStat.rb')
  #or:        statData = File.stat('fileStat.rb')
  #see also:  statData = File.lstat('fileStat.rb')
rescue 
  puts("ERROR: Could not stat fileStat.rb")
  exit(1)
end

puts("atime: #{statData.atime}")
puts("mtime: #{statData.mtime}")
puts("ctime: #{statData.ctime}")

# Most methods in File::Stat can also be accessed like this:
puts("atime: #{File.stat('fileStat.rb').atime}")
puts("mtime: #{File.stat('fileStat.rb').mtime}")
puts("ctime: #{File.stat('fileStat.rb').ctime}")

