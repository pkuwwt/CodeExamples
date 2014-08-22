#!/usr/local/bin/ruby

##
# @file      classIntrospection.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Class introspection in ruby.@EOL
# @Keywords  ruby example class introspection
# @Std       Ruby 1.8

class Person
  @@attC = 'attCV'

  def initialize(name)
    @attI  = 'attIV'
  end

  def MethI
  end

  def Person.MethC
  end
end

puts("Class Variables:    #{Person.class_variables.inspect}")

puts()

puts("Instance Variables: #{Person.instance_variables.inspect}")

puts()

puts("All Methods:        #{Person.methods.inspect}")

puts()

puts("Instance Methods:   #{Person.instance_methods.inspect}")

puts()

puts("Class Methods:      #{Person.singleton_methods.inspect}")

## See Also:
#private_instance_methods
#private_methods
#protected_instance_methods
#protected_methods
#public_instance_methods
#public_methods
#
#method_defined?
#public_method_defined?
#private_method_defined?
#protected_method_defined?
#
#instance_variable_set
#instance_variable_get
#
#instance_method
#method
#private_class_method
#public_class_method
