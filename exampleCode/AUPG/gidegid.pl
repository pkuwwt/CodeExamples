#!/usr/local/bin/perl
# -*- Mode:Perl; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      gidegid.pl
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1994 by Mitch Richling.  All rights reserved.
# @brief     How to get the GID and EGID@EOL
# @Keywords  perl GID EGID
# @Std       Perl5
#
# This is an example program intended to illustrate how to query for user GID and effective user EGID.
#            

##----------------------------------------------------------------------------------------------------------------------------------

# Figure out who the GID and EGID of the user is running this thing.
$theGid = $(;
$theEgid = $);
printf("gid: %ld  egid: %ld\n", $theGid, $theEgid);
