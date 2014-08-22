#!/usr/local/bin/python
# -*- Mode:Python; Coding:us-ascii-unix; fill-column:132 -*-
####################################################################################################################################
##
# @file      minStdRandGenPY.py
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1997,2006 by Mitch Richling.  All rights reserved.
# @brief     minimal implementation of the minimal standard random number generator@EOL
# @Keywords  none
# @Std       python
#
# See the C version for extensive algorithm notes.  This one is based upon the Perl version -- which is why it looks sorta
# non-python-like... :)

#-----------------------------------------------------------------------------------------------------------------------------------
m     = 2147483647;   # 2**(31)-1 for MSLCG
a     = 16807;        # 7**5 for MSLCG
q     = int(m / a);   # 127773 for MSLCG
r     = m % a;        #   2836 for MSLCG
randN = 1;            # Set the seed (first random number)

for x in range(10):
    randN = a * (randN % q) - r * int(randN / q);
    if randN<0 :
        randN += m;
    print '%12d' % randN;
