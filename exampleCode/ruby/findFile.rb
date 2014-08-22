#!/usr/local/bin/ruby

##
# @file      findFile.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Demonstrate the Ruby find module.@EOL
# @Keywords  find netapp directory snapshot
# @Std       Ruby 1.8
#
#            Rather like the perl find module, Ruby's find provides an
#            easy way to traverse directories.
#
#            In this example, we traverse the directory structure
#            starting with the current working directory, and count
#            all the files and directories we find.  Just to
#            demonstrate Find.prune(), and make NetApp users happy, we
#            avoid directories named .snapshot.  Note, we "find" the
#            current working directory, so the directory count will
#            always be at least 1.

require 'find'

numberOfFiles=0
numberOfDirs=0
Find.find(Dir.pwd()) do |path|
  if FileTest.directory?(path)
    numberOfDirs += 1
    if File.basename(path)[0] == ".snapshot" 
      Find.prune       # Skip this one!
    end
  else
    numberOfFiles += 1
  end
end

puts("Found #{numberOfDirs} directories\n")
puts("Found #{numberOfFiles} files\n")
