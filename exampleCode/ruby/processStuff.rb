#!/usr/local/bin/ruby

##
# @file      processStuff.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Simple process stuff like PID.@EOL
# @Keywords  ruby examples process uid gid groups
# @Std       Ruby 1.8
#
#            Several handy things are in the Process object.  In this
#            example, we demonstrate the aspects of the Process object
#            that deal with the current process.  Look at the fork
#            examples for info about fork, wait*, and detach.  Check
#            out the signal examples for UNIX signals.
#            
#            Some other useful stuff not illustrated:
#
#              Exit:            exit, exit!, abort
#              Set/get nice:    getpriority, setpriority
#              Get/set ulimits: getrlimit, setrlimit
#

# Query the current process pid and ppid
puts("My PID:  #{Process.pid}")
puts("My PPID: #{Process.ppid}")

# Query the current process uid, euid, gid, and egid.  With the
# appropriate privileges, one can set the uid, gid, euid, and egid
# with: uid=, gid=, egid=, euid=
puts("My  uid: #{Process.uid}")
puts("My euid: #{Process.euid}")
puts("My  gid: #{Process.gid}")
puts("My egid: #{Process.egid}")

# Supplemental groups may be queried (groups) and set (groups= &
# initgroups).  The max number of groups can be determined with
# maxgroups.

puts("My groups: #{Process.groups.inspect}")

# The process CPU time can be queried (user & system)
puts("Times: #{Process.times.inspect}")

