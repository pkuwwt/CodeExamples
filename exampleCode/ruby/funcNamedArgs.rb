#!/usr/local/bin/ruby

##
# @file      funcNamedArgs.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Hack for named function arguments in ruby.@EOL
# @Keywords  ruby example functions named arguments
# @Std       Ruby 1.8
#

#            Ruby 1.8 doesn't really support named arguments like
#            lisp; however, a common idiom has arisen in the Ruby
#            community for doing something similar.  That idiom is to
#            pas a hash.  Most people name the hash argument 'params'.

def namedArgFunc(arg1, params)
end

# People generally call such a function like so:

namedArgFunc('foo', 
             { 
               'nameArg1' => 1,
               'nameArg2' => 2,
             })

