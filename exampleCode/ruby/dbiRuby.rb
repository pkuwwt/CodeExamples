#!/opt/local/bin/ruby

##
# @file      dbiRuby.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Using the Ruby DBI with Ruby-like constructs.@EOL
# @Keywords  ruby example database postgresql dbi
# @Std       Ruby 1.8
#
#            This example illustrates use of the DBI in a ruby-esq
#            way.  Notice how we avoid the .finish calls that are so
#            often a source of error with the Perl DBI -- see the
#            dbiPerl.rb example.
#
#            WARNING: This example requires that you have the DBI
#            module installed along with a driver for your database.
#            The code connects to a PostgreSQL DB named 'richmit' that
#            doesn't require a password.  Also note the first like is
#            /opt/local/bin/ruby, and not /usr/local/bin/ruby -- that
#            is because that is where I have a Ruby with the DBI
#            installed!  Surely this will require some modification
#            for your particular database setup.

require 'dbi'
 
# Connect to the data base (PostgreSQL DB named "richmit" with no password).
DBI.connect('DBI:pg:richmit') do | databaseHandle |

  # Create out table.
  databaseHandle.do('create table foobar (foo varchar(16), bar numeric(10,2));')

  # Insert some records into our table.  Note the use of placeholders.
  databaseHandle.prepare("insert into foobar VALUES (?, ?)") do | dbiStatementHandle |
    dbiStatementHandle.execute("beep", "1")
    dbiStatementHandle.execute("beepbeep", "2")
  end

  # Extract the records, and display them
  databaseHandle.select_all('select * from foobar;') do | row |
    puts(row.inspect)
  end

  # Drop the table we created.
  databaseHandle.do('drop table foobar;')
  
end
