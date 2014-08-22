#!/usr/local/bin/perl
# -*- Mode:Perl; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      glob.pl
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1995 by Mitch Richling.  All rights reserved.
# @brief     How to read directory entries@EOL
# @Keywords  perl glob
# @Std       Perl5
#
# This Perl program is intended to illustrate how one reads the filenames in a directory with Perl's glob function.

##----------------------------------------------------------------------------------------------------------------------------------

$dirToRead = shift(@ARGV);

# Read the directory entries.
@dirEnts = glob("$dirToRead/* $dirToRead/.*");

# Print out the info
foreach $dirEnt (@dirEnts) {
  printf("File: %s\n", $dirEnt);
}
