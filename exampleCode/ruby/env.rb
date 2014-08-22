#!/usr/local/bin/ruby

##
# @file      env.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     How to access environment variable in ruby.@EOL
# @Keywords  ruby example environment variables
# @Std       Ruby 1.8
#
#            In a Perlish way, Ruby uses a hash called ENV to hold the
#            current script's process environment.  Changing this hash
#            has the side effect of modifying the environment a
#            sub-process will inherit.  Aside from this side effect,
#            the ENV hash is like any other hash.

print("Found #{ENV.keys.size} environment variables\n\n")

print("ENVS: #{ENV.keys.inspect}\n\n")

print("ENV['SHELL']=#{ENV['SHELL']}\n\n")

print("The FOO variable: #{`env | grep FOO`.chop}\n")
print("Now setting FOO=BAR\n")
ENV['FOO'] = 'BAR'
print("The FOO variable: #{`env | grep FOO`.chop}\n\n")
