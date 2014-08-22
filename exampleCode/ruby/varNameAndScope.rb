#!/usr/local/bin/ruby

##
# @file      varNameAndScope.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Variable names and scope Ruby.@EOL
# @Keywords  ruby example variables names
# @Std       Ruby 1.8

# Local vars start with a lower case letter or underscore
localVariable = 1

# Global variables are prefixed with a $
$globalVariable = 1

# Instance variables start with @
@anInstanceVarable = 1

# Class variables start with @@
@@aClassVariable = 1

# Class names, module names, and constants start with upper case.
# Most people think constants should be ALL_UPPERCASE.
ConstantsStartWithCaps = 3.14
