#!/usr/local/bin/perl
# -*- Mode:Perl; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      uideuid.pl
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1994 by Mitch Richling.  All rights reserved.
# @brief     UID and EUID in perl.@EOL
# @Keywords  UID EUID Perl
# @Std       Perl5 POSIX, BSD4.3, SVID3
#
# This is an example program intended to illustrate how to query for user ID and effective user ID.

##----------------------------------------------------------------------------------------------------------------------------------

# Figure out who the user is running this thing.
$theUid = $<;
$theEuid = $>;
printf("uid: %ld  euid: %ld\n", $theUid, $theEuid);
