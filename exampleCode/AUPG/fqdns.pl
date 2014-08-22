#!/usr/local/bin/perl
# -*- Mode:Perl; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      fqdns.pl
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
# @brief     How to get a host's FQDNS.@EOL
# @Keywords  FQDNS resolver hostname gethostbyname gethostbyaddr
# @Std       Perl5
#
# Getting the FQDNS from a perl program is not as simple as it should be for many UNIX platforms.  The difficulty stems from the
# fact that many UNIX platforms do not return FQDNS from a gethostbyname() call.  The related system call, gethostbyaddr() will
# always return the FQDNS, but you must know the host's IP address to use it.  The solution is to first get the IP address
# associated with a partial host name and then get the FQDNS name associated with it.

##----------------------------------------------------------------------------------------------------------------------------------

$ENV{'PATH'}='/bin:/usr/bin:/usr/local/bin';

# Get the local host name
$hostName = `hostname`;
chomp($hostName);

# Lookup name and address
($name,$aliases,$addrtype,$length,@addrs) = gethostbyname($hostName);
($name,$aliases,$addrtype,$length,@addrs) = gethostbyaddr($addrs[0], $addrtype);
($a,$b,$c,$d) = unpack('C4',$addrs[0]);

print "Name: $name Address 1: $a.$b.$c.$d \n";
