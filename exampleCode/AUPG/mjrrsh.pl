#!/usr/local/bin/perl
# -*- Mode:Perl; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      mjrrsh.pl
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2003 by Mitch Richling.  All rights reserved.
# @brief     an rsh work alike@EOL
# @Keywords  perl TCP/IP rsh UNIX
# @Std       Perl5
#
# Any UNIX admin that considers security at even the most basic level should take a close look at a text that describes basic TCP/IP
# protocols used for UNIX.  In particular, any good UNIX admin should take a close look at the protocols used by the "r"-commands.
#
# This perl script is intended to illistrate just how simple rsh(2) realy is.  Note, that the network protocol simply has the user
# id and the command being sent to the remote host.  Then the remote host sends the results back.  Not a very good idea in our
# dangerous world.
#
# The complexity in the script given is intended to provide an example of how to make a perl script timeout and exit gracefully even
# in the face of network protocols that can just hang forever.  The script is a good basis for a custom rsh that can be used as a
# more robust version than the one supplied by most OS venders.  One can achieve the same effect by putting an alarm in a perl
# script and using the OS version of rsh.  The only disadvantage of this approach is that the rsh can be left hanging around from
# such a program.  By rolling your own version of rsh in a perl script you can avoid shelling out, and you can cleanly handle
# errors.  As an additional advantage, you can take different actions based upon where in the script the problem occurs.

##----------------------------------------------------------------------------------------------------------------------------------

use IO::Socket;

$DNSQTIMEOUT = 10;
$PINGTIMEOUT = 10;
$PORTTIMEOUT = 10;
$SENDTIMEOUT = 10;
$RECVTIMEOUT = 10;

if( ($< != 0) || ($> != 0) )  {
    print "Real or Effective UID is not 0\n"; 
    exit(1);
}

if( $#ARGV != 2) {
    die "Incorrect argument count.\n"; 
    exit(2);
}

$SIG{ALRM} = sub { print "Timeout during DNS lookup.\n"; exit(3); };
alarm $DNSQTIMEOUT;
if( ! gethostbyname($ARGV[1])) {
    print "Unknown host.  DNS lookup failed\n"; 
    exit(4);
}

$SIG{ALRM} = sub { print "Ping command hung.\n"; exit(5); };
alarm $PINGTIMEOUT;
if( !(`/usr/sbin/ping $ARGV[1] 1` =~ m/is alive/)) {
    print "Couldn't ping host.\n"; 
    exit(6);
}

$SIG{ALRM} = sub { print "Timeout obtaining a socket.\n"; exit(7); };
alarm $PORTTIMEOUT;
$tryPort = 1023;
$socket = 0;
while(($tryPort > 128) && (! $socket)) {
    $socket = IO::Socket::INET->new(PeerAddr => "$ARGV[1]",
                            PeerPort => 514,
                LocalPort => $tryPort,
                            Proto => "tcp",
                Type => SOCK_STREAM);
    $tryPort--;
}

if($socket) {
    $SIG{ALRM} = sub { print "Timeout sending command.\n"; exit(8); };
    alarm $SENDTIMEOUT;
    print $socket "\000$ARGV[0]\000$ARGV[0]\000$ARGV[2]\000";
    $SIG{ALRM} = sub { print "Timeout receving command output.\n"; exit(9); };
    alarm $RECVTIMEOUT;
    while(<$socket>) 
        { print "$_"; }
    close($socket);
    exit(0);
} else {
    print "Couldn't get socket\n";
    exit(10);
}
