#!/usr/local/bin/perl
# -*- Mode:Perl; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      arguments.pl
# @author    Mitch Richling <http://www.mitchr.me>
# @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
# @brief     How to access command line arguments.@EOL
# @Keywords  Perl command line arguments ARGV
# @Std       Perl5
#
# This program works in any environment that lets perl have command line arguments.
#            

##----------------------------------------------------------------------------------------------------------------------------------

printf("Program has %d argument%s (\$#ARGV=%d)\n", $#ARGV + 1, ($#ARGV == 0 ? "" : "s"), $#ARGV);

# Note that $ARGV[0] is the first ARGUMENT, and not the name of the program.
for($i=0;$i<=$#ARGV;$i++) {
  printf("Argument #%02d: %s\n", $i, $ARGV[$i]);
}
