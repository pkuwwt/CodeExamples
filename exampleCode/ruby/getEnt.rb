#!/usr/local/bin/ruby

##
# @file      getEnt.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     How to get user password and group info.@EOL
# @Keywords  ruby examples passwd password group
# @Std       Ruby 1.8
#
#            The Ruby standard library Etc class provides access to
#            the passwd and group database.  Entry points for the
#            POSIX functions getpwnam, getgrgid, getpwuid, and
#            getpwnam are all provided, but they return Ruby objects.
#            In addition more Ruby-like iterators are also provided
#            for the password and group databases.  Finally, a very
#            traditional interface consisting of endgrent, endpwent,
#            getgrent, getpwent, setgrent, and setpwent is provided
#            for old school, POSIX-style access.
#
#            Members of group objects
#                  name     Group name   String 
#                  passwd   Password     String
#                  gid      Group ID     Integer
#                  mem      Members      Array of strings
#
#            Members of passwd objects
#                  name     Login Name   String
#                  passwd   Password     String
#                  uid      User ID      Integer
#                  gid      Group ID     Integer
#                  gecos    GECOS        String
#                  dir      Home Dir     String
#                  shell    Shell        String
#
#            See the processStuff.rb example for details about
#            functions like initgroups & groups.

require 'etc' 

puts("Login Name: #{Etc.getlogin}")

# Get passwd record from login name
p1=Etc.getpwnam(Etc.getlogin)
puts("User ID: #{p1.uid}")
puts("Group ID: #{p1.gid}")

# Get group name via the GID for the current user
g1=Etc.getgrgid(p1.gid)
puts("Group Name: #{g1.name}")

# Get the user name via the UID for the current user
#for the 0 user (usually 'root')
p2=Etc.getpwuid(p1.uid);
puts("Login Name: #{p2.name}")

# A "Ruby-like" construct is provided by the group iterator:
numGroups=0;
Etc.group {|g|
  numGroups += 1;
}
puts("Found #{numGroups} groups")

# A similar construct is provided by the passwd iterator:
numPasswds=0;
Etc.passwd {|p|
  numPasswds += 1;
}
puts("Found #{numPasswds} passwds")

# Get the name of the "SuperUser" (UID 0) if the system has one...
begin
  puts("SuperUser Name: #{Etc.getpwuid(0).name}")
rescue
  puts("This system has no 'root' user!")
end
