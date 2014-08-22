#!/usr/local/bin/ruby

##
# @file      classAttributeAccess.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Attribute, and virtual attribute, access.@EOL
# @Keywords  ruby example class virtual attribute reader writer accessor
# @Std       Ruby 1.8

class Person
  def initialize(name, weight) # Constructor
    @name   = name
    @weight = weight
  end

  attr_reader :name, :weight
  attr_writer :weight

  ### The two lines above, are the same as doing the following:
  # def name
  #   @name
  # end
  # def weight
  #   @weight
  # end
  # def weight=(newWeight)
  #   @weight = newWeight
  # end

  # Note that "attr_accessor :weight" is the faster way to create
  # read/write attributes, and is the same as using "attr_reader
  # :weight" with "attr_writer :weight"

  # The following two methods provide access for a read/write "virtual
  # attribute".  This is a powerful technique, and common Ruby idiom!
  def weightKG
    @weight * 0.45359237
  end
  def weightKG=(newWeightKG)
    @weight = newWeightKG / 0.45359237
  end

end

mitch = Person.new('Mitch', 250)

puts("Initial Values:")
puts("  Mitch's Name: #{mitch.name}\n")
puts("  Mitch's Wt:   #{mitch.weight}lb\n")
puts("  Mitch's Wt:   #{mitch.weightKG}kg\n")

mitch.weight = 200
puts("After Change:")
puts("  Mitch's Wt:   #{mitch.weight}lb\n")
puts("  Mitch's Wt:   #{mitch.weightKG}kg\n")

mitch.weightKG = 200
puts("Final Change:")
puts("  Mitch's Wt:   #{mitch.weight}lb\n")
puts("  Mitch's Wt:   #{mitch.weightKG}kg\n")
