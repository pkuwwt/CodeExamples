#!/usr/local/bin/ruby

##
# @file      funcArgNamed.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Hack for named function arguments in Ruby.@EOL
# @Keywords  ruby example functions named arguments
# @Std       Ruby 1.8
#
#            Ruby 1.8 doesn't directly support named arguments like
#            lisp; however, a common idiom has developed in the Ruby
#            community for doing something similar.  That idiom is to
#            pass a hash as the last argument.  Most of the time this
#            has argument is named 'params'.

def namedArgFunc(arg1, params)
  puts("First Argument: #{arg1}")
  params.each do |k,v|
    puts("Named Arg (#{k}): #{v}")
  end
end

# People generally call such a function like so:

namedArgFunc('foo', 
             { 
               'nameArg1' => 1,
               'nameArg2' => 2,
             })

