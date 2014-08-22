#!/opt/local/bin/ruby

##
# @file      dbiPerl.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Using the Ruby DBI with Perl-like constructs.@EOL
# @Keywords  ruby example database postgresql dbi
# @Std       Ruby 1.8
#
#            This example illustrates use of the DBI in a relatively
#            perl-esq way.  See the dbiRuby.rb example for a more
#            Ruby-Like example using things like iterators and
#            code blocks.
#
#            WARNING: This example requires that you have the DBI
#            module installed along with a driver for your database.
#            The code connects to a PostgreSQL DB named 'richmit' that
#            doesn't require a password.  Also note the first line is
#            /opt/local/bin/ruby, and not /usr/local/bin/ruby -- that
#            is because that is where I have a Ruby with the DBI
#            installed!  Surely this will require some modification
#            for your particular database setup.

require 'dbi'
 
# First we need to connect to the DB (a PostgreSQL DB named richmit)
databaseHandle = DBI.connect('DBI:pg:richmit')

# Create out table -- no "prepare" or "execute", it just works...
databaseHandle.do('create table foobar (foo varchar(16), bar numeric(10,2));')

# Now we put some data into the table.  The Ruby DBI has an
# interesting feature called "placeholders".  Kool stuff..
sqlCode = "insert into foobar VALUES (?, ?)"
databaseHandle.do(sqlCode, "beep", "1")
databaseHandle.do(sqlCode, "beepbeep", "2")

# Similarly, the execute method supports placeholders too
dbiStatementHandle = databaseHandle.prepare(sqlCode)
dbiStatementHandle.execute("beepbeepbeep", "3")
dbiStatementHandle.execute("beepbeepbeepbeep", "4")
dbiStatementHandle.finish

# Let's get our data out -- do the prepare/execute thing again just to
# illustrate the technique...
dbiStatementHandle = databaseHandle.prepare('select * from foobar')
dbiStatementHandle.execute

# Print output each row we get from the select
while row=dbiStatementHandle.fetch do
  puts(row.inspect)
end

#Close off the handle (don't forget to do this!!)
dbiStatementHandle.finish

# Drop the table we created.
databaseHandle.do('drop table foobar;')

# Before we exit, it is nice to disconnect
databaseHandle.disconnect
