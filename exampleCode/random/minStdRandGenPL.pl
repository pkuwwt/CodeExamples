#!/usr/local/bin/perl
# -*- Mode:Perl; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
##
# @file      minStdRandGenPL.pl
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1997 by Mitch Richling.  All rights reserved.
# @brief     minimal implementation of the minimal standard random number generator@EOL
# @Keywords  none
# @Std       Perl5
#
#            See the C version for extensive algorithm notes.
#            

#-----------------------------------------------------------------------------------------------------------------------------------
$m     = 2147483647;   # 2**(31)-1 for MSLCG
$a     = 16807;        # 7**5 for MSLCG
$q     = int($m / $a); # 127773 for MSLCG
$r     = $m % $a;      #   2836 for MSLCG
$randN = 1;            # Set the seed (first random number)

for($i=0; $i<10; $i++) {
  $randN = $a * ($randN % $q) - $r * int($randN / $q);
  if($randN<0) {
    $randN += $m;
  }
  printf("%12ld\n", $randN);
}
