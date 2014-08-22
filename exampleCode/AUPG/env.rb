#!/usr/local/bin/ruby
# -*- Mode:Ruby; Coding:us-ascii-unix; fill-column:132 -*-

####################################################################################################################################
# @file      env.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
# @brief     How to access environment variables@EOL
# @Keywords  UNIX shell environment variable ruby
# @Std       Ruby1.8
#
# This Ruby program is intended to illustrate how one can query and modify environment variables.

##----------------------------------------------------------------------------------------------------------------------------------

# One way to query the SHELL environment variable.
printf("SHELL=%s\n", ENV['SHELL']);

# To tell the diffrence between a NULL value and an undefined environment variable:
if(ENV.member?('FOO')) then
  printf("The SHELL variable is set to: %s\n", ENV['FOO']);
else
  print "The variable FOO was NOT defined.\n";
end

# You can clear a variable like this:
printf("Clearing SHELL\n");
ENV.delete('SHELL')
printf("SHELL=%s\n", ENV['SHELL']);

# You can Set an environment variable like this:
printf("Setting SHELL=/bin/foo\n");
ENV['SHELL'] = '/bin/foo';
printf("SHELL=%s\n", ENV['SHELL']);

# You can add something to the PATH environment variable like so:
printf("PATH=%s\n", ENV['PATH']);

# This is complex because we need a : if PATH is non-empty, otherwise we only need to set PATH equal to what we want.
ENV['PATH'] += ((ENV['PATH'] ? ':' : '') + '/usr/local/bin');
printf("PATH=%s\n", ENV['PATH']);
