#!/usr/local/bin/ruby

##
# @file      signals.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Send/receive UNIX signals.@EOL
# @Keywords  ruby example signal kill
# @Std       Ruby 1.8
#
#            Receiving UNIX signals.  See kill.rb for how to send 
#            signals.
#            

sigIntHand = proc { puts("SIG RECV: SIGINT") }

# Now we attach our handler to SIGINT.
Signal.trap("SIGINT", sigIntHand)

# One can also just hand trap a bit of code..
Signal.trap("SIGQUIT") { puts("SIG RECV: SIGQUIT") }

# You can ignore a signal too..
Signal.trap("SIGUSR1", "IGNORE")

# We can restore the default signal handler too..
Signal.trap("SIGUSR1", "DEFAULT")

# Lastly, we can store the old handler when setting a new one
oldHandler = trap("INFO") { puts("SIG RECV: SIGINFO") }
# And we can restore the handler like this:
trap("INFO", oldHandler)

# One can get a list of signal numbers and names with list:
puts(Signal.list.inspect)

# Now we wait for some signals...
puts("Send me a SIGINT for a message:   kill -SIGINT  #{Process.pid}")
puts("Send me a SIGQUIT for a message:  kill -SIGQUIT #{Process.pid}")
puts("Send me a SIGKILL for me to stop: kill -SIGKILL #{Process.pid}")
puts("The kill.rb example is a good way to send me some signals..")
while(true)
    sleep 30
end
