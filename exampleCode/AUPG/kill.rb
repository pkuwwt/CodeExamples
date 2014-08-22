#!/usr/local/bin/ruby
# -*- Mode:Ruby; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      kill.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
# @brief     How to send signals with ruby.@EOL
# @Keywords  UNIX ruby example
# @Std       Ruby 1.8.1
#
# This Ruby program is intended to illistrate how to send signals to other processes.  This is a very simple version of the kill(1)
# UNIX command.

##----------------------------------------------------------------------------------------------------------------------------------

if(ARGV.length != 2) then
  puts("ERROR: Incorrect number of arguments.");
  puts("Use: kill -n m");
  puts("     n is a number indicateing the signal");
  puts("     m is the PID of the process to send the signal to.");
  exit(1);
end

# Check to see if they put a '-' in front of the signal number, and if they did, we extract the signal number.
signalNumber = nil
if(tmp = ARGV[0].match(/^-([0-9]+)$/)) then
  signalNumber = tmp[1].to_i
else
  printf("ERROR: Bad first argument.\n")
  exit(1);
end

# Make sure we got a number, and extract it.  This is the PID we will signal.
thePid = nil
if(tmp = ARGV[1].match(/^([0-9]+)$/)) then
  thePid = tmp[1].to_i
else
  printf("ERROR: Bad second argument.\n");
  exit(1);
end

# Send the signal.
Process.kill(signalNumber, thePid)

