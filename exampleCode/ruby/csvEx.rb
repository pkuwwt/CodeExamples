#!/usr/local/bin/ruby

##
# @file      csvEx.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2008 by Mitch Richling.  All rights reserved.
# @Revision  $Revision$ 
# @SCMdate   $Date$
# @brief     Simple example of the CSV module.@EOL
# @Keywords  
# @Std       Ruby 1.8
#
#            The CSV module provides an easy way to produce and
#            consume properly formed CSV data -- including transparent
#            handling of complexities like comma escaping.  Support
#            for higher level functionality like column names is not
#            provided.

require "csv"

# Convert an array into a CSV string
a = CSV.generate_line([1,2,'a,b',3])
puts(a.inspect)

# Convert a string containing a line of CSV data into an array
b = CSV.parse_line(a)
puts(b.inspect)

# Creating a CSV file is easy
CSV.open('data.csv', 'w') do |writer|
  # The normal way to add rows is with <<
  1.upto(2) do |i|    
    writer << [i, i*2, i*2+1]
  end
  # One can add rows with .add_row too.
  5.upto(7) do |i|    
    writer.add_row([i, i*2, i*2+1])
  end
end

# Read each line, convert it to an array, and pass the array to row...
CSV.open('data.csv', 'r') do |row|
  puts("METHOD 1 ::: " + row.inspect)
end

# The 'reader' object may be iterated  over to read the file...
csvReader = CSV.open('data.csv', 'r')
csvReader.each do |row|  
  puts("METHOD 2 ::: " + row.inspect)
end
csvReader.close

# Finally, we can "shift" the rows off the reader object 
csvReader = CSV.open('data.csv', 'r')
loop do
  row = csvReader.shift
  if (row.empty?) then
    break
  end
  puts("METHOD 3 ::: " + row.inspect)
end
csvReader.close
