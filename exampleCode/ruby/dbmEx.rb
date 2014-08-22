#!/usr/local/bin/ruby

##
# @file      dbmEx.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Ruby DBM example (GDBM actually).@EOL
# @Keywords  
# @Std       Ruby 1.8
#
#            Simple demonstration of how to use the DBM-like APIs in
#            the Ruby standard library.  Some minor differences exist
#            between dbm , sdbm, and gdbm.  For example, dbm will
#            automatically name the files with a .db extension while
#            gdbm will not.  This example uses gdbm.
#
#            As with most Ruby APIs the old fashioned open/stuff/close
#            pattern is supported; however, this example only
#            illustrates the more ruby-esq way using code blocks.
#
#            If the DB file exists, then we read it and display its
#            contents.  If it doesn't exist, then we create it and put
#            some data into the file.  Run this program twice to see
#            everything it can do.

require 'gdbm'

if FileTest.exist?("foo.db") then
  GDBM.open("foo.db", GDBM::READER) do |aDBMobj|
    puts("Keys: #{aDBMobj.keys.inspect}")
    aDBMobj.each do |k,v|
      puts("GDBM[#{k}]=#{v}")
    end
  end
else
  GDBM.open("foo.db", GDBM::NEWDB) do |aDBMobj|
    aDBMobj["abc"] = "ABC"
    aDBMobj["def"] = "DEF"
    puts("Created file, and added two key/value pairs")
  end
end


