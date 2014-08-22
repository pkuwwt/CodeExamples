#!/usr/local/bin/ruby

##
# @file      tkHello.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Hello world program with Ruby using Tk.@EOL
# @Keywords  ruby example tk 
# @Std       Ruby 1.8
#
#            tk is the most commonly used GUI toolkit in Ruby;
#            however, this appears to be more because it is always
#            available rather than it being the best available.  This
#            little program is a prototypical "Hello World" program
#            using Ruby's Tk.

require 'tk'

root = TkRoot.new { title "Ruby Tk Example 01" }

TkLabel.new(root) {
   text  'Hello, World!'
   pack  { padx 15 ; pady 15; side 'left' }
}

Tk.mainloop
