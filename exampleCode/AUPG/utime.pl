#!/usr/local/bin/perl
# -*- Mode:Perl; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      utime.pl
# @author    Mitch Richling <http://www.mitchr.me>
# @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
# @brief     How to change mtime and atime on a file.@EOL
# @Keywords  timestamp touch file atime mtime perl
# @Std       Perl5

##----------------------------------------------------------------------------------------------------------------------------------

if($#ARGV != 0) {
  printf("ERROR: A file name is required as the single argument.\n");
  exit(1);
}

$atime = 0;
$mtime = 0;

utime($atime, $mtime, $ARGV[0]);
