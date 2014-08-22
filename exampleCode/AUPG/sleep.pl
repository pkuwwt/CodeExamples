#!/usr/local/bin/perl
# -*- Mode:Perl; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      sleep.pl
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
# @brief     Perl and sleeping@EOL
# @Keywords  sleep signal perl
# @Std       Perl5
#
# This Perl program is intended to illustrate the correct way to make a program sleep for at least n seconds.  This is more complex
# that simply calling the sleep() function.

##----------------------------------------------------------------------------------------------------------------------------------

$secToSleep = 600;

# Setup a signal handler so we can test with SIGUSR1 signals
$SIG{'USR1'} = sub {};
printf("My PID(try SIGUSR1): %ld\n", $$);

$startTime = time();
$endTime = $startTime + $secToSleep;

# Loop until we have waited secToSleep seconds.
printf("Current Time=%ld\n", time());
while (time() <= $endTime) {
  sleep($endTime - time());
  printf("Current Time=%ld\n", time());
}

printf("Time gone by: %ld\n", time() - $startTime);
