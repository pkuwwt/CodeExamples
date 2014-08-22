#!/usr/local/bin/perl
# -*- Mode:Perl; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      readdir.pl
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1994 by Mitch Richling.  All rights reserved.
# @brief     UNIX directory access.@EOL
# @Keywords  UNIX filename directory UFS Perl opendir readdir
# @Std       Perl5
#
# This Perl program is intended to illustrate how one reads UNIX directories(filenames in a subdirectory) with readdir.  Perl's glob
# function makes this method a bit archaic if all one wants to do is to know the names of files in some directory...

##----------------------------------------------------------------------------------------------------------------------------------

$dirToRead = shift(@ARGV);

# Open the directory handle
opendir(DH, $dirToRead) || die "Could not open up $dirToRead\n";

# Read the directory entries.  Perl's readdir function will read ALL the entries if it is called in a list context.  Kool!
@dirEnts = readdir(DH);
closedir(DH);

# No loop was required to read the data, so we have a loop here to print it all out. :)
foreach $dirEnt (@dirEnts) {
  printf("File: %s\n", $dirEnt);
}
