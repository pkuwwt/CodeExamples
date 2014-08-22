#!/usr/local/bin/perl
# -*- Mode:Perl; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      kill.pl
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
# @brief     How to send signals with perl@EOL
# @Keywords  UNIX perl example
# @Std       Perl5
#
# This Perl program is intended to illistrate how to send signals to other processes.  This is a very simple version of the kill(1)
# UNIX command.

##----------------------------------------------------------------------------------------------------------------------------------

if($#ARGV != 1) {
  printf("ERROR: Incorrect number of arguments.\n");
  printf("Use: kill -n m\n");
  printf("     n is a number indicateing the signal\n");
  printf("     m is the PID of the process to send the signal to.\n");
  exit(1);
}

# Check to see if they put a '-' in front of the signal number, and if they did, we extract the signal number.
if($ARGV[0] =~ m/^-([0-9]+)$/) {
  $signalNumber = $1;
} else {
  printf("ERROR: Bad first argument.\n");
  exit(1);
}

# Make sure we got a number, and extract it.  This is the PID we will
# signal.
if($ARGV[1] =~ m/^([0-9]+)$/) {
  $thePid = $1;
} else {
  printf("ERROR: Bad second argument.\n");
  exit(1);
}

# Send the signal.
if(kill($signalNumber, ($thePid)) < 1) {
  printf("ERROR: kill failed.\n");
  exit(1);
}
