#!/usr/local/bin/ruby

##
# @file      fileTest.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     File tests with Ruby.@EOL
# @Keywords  ruby example files filetest
# @Std       Ruby 1.8
#
#             Ruby has a ton of file tests in the FileTest module.
#             They are also magically exposed in the File class too.
#             Here is a list in alphabetical order:
#
#                 FileTest.blockdev?(file_name)
#                 FileTest.chardev?(file_name)
#                 FileTest.directory?(file_name)
#                 FileTest.executable?(file_name)
#                 FileTest.executable_real?(file_name)
#                 FileTest.exist?(file_name)
#                 FileTest.file?(file_name)
#                 FileTest.grpowned?(file_name)
#                 FileTest.identical?(file_1, file_2)
#                 FileTest.owned?(file_name)             -- do I own file?
#                 FileTest.pipe?(file_name)
#                 FileTest.readable?(file_name)
#                 FileTest.readable_real?(file_name)
#                 FileTest.setgid?(file_name)
#                 FileTest.setuid?(file_name)
#                 FileTest.size(file_name)
#                 FileTest.size?(file_name)              -- nil if missing, integer other wise
#                 FileTest.socket?(file_name)
#                 FileTest.sticky?(file_name)
#                 FileTest.symlink?(file_name)
#                 FileTest.writable?(file_name)
#                 FileTest.writable_real?(file_name)
#                 FileTest.zero?(file_name)

# Here we test the current working directory
if(FileTest.executable?('.') and FileTest.directory?('.')) 
  puts("CWD is is executable and a directory!")
else
  puts("Something very wrong happened. :)")
end


