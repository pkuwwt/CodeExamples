#!/usr/local/bin/perl
# -*- Mode:Perl; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      passwd.pl
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1997 by Mitch Richling.  All rights reserved.
# @brief     Password database access in Perl.@EOL
# @Keywords  UNIX password passwd database getpwent
# @Std       Perl5
#
# Cats the password data base to standard out.  Works with whatever name service is configured because it makes use of the
# getpwent() function, and that function is implemented with the system call getpwent().

##----------------------------------------------------------------------------------------------------------------------------------

$i=0;
while(($name,$passwd,$uid,$gid,$quota,$comment,$gcos,$dir,$shell) = getpwent()) {
  $i++;
  printf("User #%05d: %s:%s:%ld:%ld:%s:%s:%s\n", $i,
         $name, $passwd, $uid, $gid, $gcos, $dir, $shell);
}
