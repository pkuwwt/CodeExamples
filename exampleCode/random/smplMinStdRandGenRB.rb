#!/home/richmit/bin/verGo.sh ruby
# -*- Mode:Ruby; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
##
# @file      smplMinStdRandGenRB.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1997,2006 by Mitch Richling.  All rights reserved.
# @brief     minimal implementation of the minimal standard random number generator@EOL
# @Keywords  none
# @Std       ruby18
#
# This is a trivial implementation of the minimal standard random number generator that uses Ruby's arbitrary precision arithmetic.
# See the alternate implementation with 32-bit arithmetic found in minStdRandGenRB.rb.
#            

#-----------------------------------------------------------------------------------------------------------------------------------
m     = 2147483647;   # 2**(31)-1 for MSLCG
a     = 16807;        # 7**5 for MSLCG
b     = 0
randN = 1;            # Set the seed (first random number)

10.times do
  randN = (randN * a + b) % m
  printf("%12d\n", randN)
end
