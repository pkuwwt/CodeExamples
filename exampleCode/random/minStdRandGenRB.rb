#!/home/richmit/bin/verGo.sh ruby
# -*- Mode:Ruby; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
##
# @file      minStdRandGenRB.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1997,2006 by Mitch Richling.  All rights reserved.
# @brief     minimal implementation of the minimal standard random number generator@EOL
# @Keywords  none
# @Std       ruby18
#
# See the C version for extensive algorithm notes.  This one is based upon the Perl version -- which is why it looks so
# non-ruby-like..
#            

#-----------------------------------------------------------------------------------------------------------------------------------
m     = 2147483647;   # 2**(31)-1 for MSLCG
a     = 16807;        # 7**5 for MSLCG
q     = (m / a).to_i; # 127773 for MSLCG
r     = m % a;        #   2836 for MSLCG
randN = 1;            # Set the seed (first random number)

10.times do
  randN = a * (randN % q) - r * (randN / q).to_i;
  if(randN<0)
    randN += m;
  end
  printf("%12d\n", randN)
end
