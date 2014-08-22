#!/usr/local/bin/ruby

##
# @file      fileOperations.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Simple file operations with Ruby.@EOL
# @Keywords  Ruby examples file operations
# @Std       Ruby 1.8
#
#            OK.  So this really isn't an example Ruby program.  It is
#            just a list of methods in File. No need to really
#            demonstrate them, but it is handy to have them all listed
#            here. :)
#
#            See the fileutils module for an alternative
#            implementation for most of the methods documented here.
#
#            File Operations in File:
#              File.compare(from, to, verbose = false) => File contents are the same
#              File.chmod(mode_int, file_name, ...)
#              File.chown(owner_int, group_int, file_name,..) -- nil for owner/group keeps them same
#              File.copy(from, to, verbose = false)
#              File.delete(file_name, ...)
#              File.delete(file_name, ...)
#              File.lchmod(mode_int, file_name, ...)
#              File.lchown(owner_int, group_int, file_name,..)
#              File.rename(old_name, new_name)
#              File.safe_unlink(*files)
#              File.truncate(file_name, length)
#              File.unlink(file_name, ...)
#              File.unlink(file_name, ...)
#              File.utime(atime, mtime, file_name,...) => Sets the access and modification times
