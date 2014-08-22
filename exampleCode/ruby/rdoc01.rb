#!/usr/local/bin/ruby

# == Synopsis
#
# @file      rdoc01.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     How to use RDoc usage for -h options.@EOL
# @Keywords  ruby example rdoc usage
# @Std       Ruby 1.8
#
#  
#
# This is an example program that doesn't really do anything
# Except illustrate how one may use RDoc to print program
# usage from an RDoc comment.
#
# == Usage
#
#    ruby rdoc.rb [ -h | -- help] other_options
#
#    -h     -- print help
#    --help -- same thing dude
#
# == Author
#  Mitch Richling
#
#

require 'rdoc/usage'

if(ARGV.member?('-h') || ARGV.member?('--help'))
  RDoc::usage('usage')
else
  print("Use -h for help\n")
end
