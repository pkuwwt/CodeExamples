#!/usr/local/bin/perl
# -*- Mode:Perl; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      group.pl
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
# @brief     How to get group data@EOL
# @Keywords  perl group unix grgrent
# @Std       Perl5
#
# Cats the group table.  Works with the defined name service because it uses the getgrent function which is implemented using the
# getgrent system call.

##----------------------------------------------------------------------------------------------------------------------------------

while(($name,$passwd,$gid,$members) = getgrent) {
  print "$name:$passwd:$gid:$members\n";
}
