#!/usr/local/bin/ruby

##
# @file      loops01.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Various loop constructions in Ruby.@EOL
# @Keywords  ruby example loop each for times upto while
# @Std       Ruby 1.8

puts("LOOP 1")
i=1
while i<5
  puts(i)
  i = i + 1
end

puts("LOOP 2")
(2..5).each {|i| puts(i)}

puts("LOOP 3")
(2...5).each {|i| puts(i)}

puts("LOOP 4")
3.upto(5) {|i| puts(i)}

puts("LOOP 5")
5.downto(3) {|i| puts(i)}

puts("LOOP 6")
5.times { |i| puts(i)}

puts("LOOP 7")
for i in 1..4 do
  puts(i)
end

puts("LOOP 8")
for i in 1..4
  puts(i)
end

puts("LOOP 9")
i=1
loop do
  puts(i)
  if(i>3) then break end
  i = i + 1
end

puts("LOOP 10")
i=1
begin
  puts(i)
  i = i + 1
end while (i<3)

puts("LOOP 11")
i=1
begin
  puts(i)
  i = i + 1
end until i>3
