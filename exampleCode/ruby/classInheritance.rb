#!/usr/local/bin/ruby

##
# @file      classInheritance.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Class inheritance in ruby.@EOL
# @Keywords  ruby example class inheritance
# @Std       Ruby 1.8

class Person
  def initialize(name) # Constructor
    @name = name
  end

  def sayMyName() # Method
    "#{@name}"
  end

  def changeName(name)
    @name = name
  end

end

class Man < Person # Man inherits from Person
  def initialize(name)
    super(name)
  end

  def sayMyName() # Method
    "Mr. #{super()}"   # Call the parent "sayMyName"
  end
end

class Woman < Person
  def initialize(name)
    super(name)
  end

  def sayMyName() # Method
    "Ms. #{@name}"  # Just access the parent @name member.
  end

end

mitch = Man.new('Mitch')
puts(mitch.sayMyName)

janie = Woman.new('Janie')
puts(janie.sayMyName)

andro = Person.new('Andro')
puts(andro.sayMyName)

janie.changeName('Pookie')
puts(janie.sayMyName)
