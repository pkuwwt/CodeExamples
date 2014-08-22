#!/usr/local/bin/ruby

##
# @file      yamlEx.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2008 by Mitch Richling.  All rights reserved.
# @Revision  $Revision$ 
# @SCMdate   $Date$
# @brief     Using the YAML module.@EOL
# @Keywords  ruby yaml
# @Std       Ruby 1.8
#
#            YAML is a "markup language" well suited for representing
#            the list and dictionary data structures commonly used in
#            modern programming languages.  YAML uses a simple,
#            efficient syntax requiring little adornment of
#            represented data types and structures.  YAML's ability to
#            represent sophisticated hierarchical data structures
#            without the complexity of XML make it a popular choice.
#            When CSV is too little, and XML is too much, YAML may be
#            the perfect choice -- data serialization in scripting
#            languages is a good example.
#            
#            JSON, which uses javascript syntax, is a very similar
#            language designed for data serialization.  In most
#            respects, JSON is a subset of YAML consisting of YAML's
#            "inline" notation.


require "yaml"

pfxStr = "  >  "

theObj = ["one", 2, "three", { "the answer" => [ 4, 2 ], "brown" => "cow" }, 'last' ]

puts("Starting Object:")
puts(pfxStr + theObj.inspect)

yamlString = YAML::dump(theObj)
puts("The YAML String:")
puts(yamlString.gsub(/^/, pfxStr))

yamlString = "---\n- 1\n- 2\n---\n- one\n- two\n"

YAML::each_document(yamlString) do |anObj|
  puts("Decoded Object:")
  puts(pfxStr + anObj.inspect)
end

# Most of the time, only a hand full of API elements are required for YAML use:
#
#  - .load(io)                 -- load FIRST document from I/O stream or string
#  - .load_documents(io, proc) -- load all docs, passing each in turn to proc
#  - .each_document            -- same
#  - .load_file(fileName)      -- load FIRST document from named file
#  - .dump(obj, io=nil)        -- dump object to I/O stream (or return string if io==nil)

# Example YAML Documents
#
# # --- # List Of Numbers
# # - 1
# # - 2
# # - 3
# 
# # --- # List Of Strings 1
# # - "1"
# # - "2"
# # - "3"
# 
# # --- # List Of Strings 2
# # - "1"
# # - two
# # - "3"
# 
# # --- # Hash (string keys, number values)
# # - one: 1
# # - two: 2
# 
# # --- # Hash (number keys, string values)
# # - 1: one
# # - 2: two
# 
# # --- # Hash (everything is a string)
# # - "1": one
# # - two: "2"
# # ...
# 
# # --- # List of lists
# # - 1
# # - 2
# # - 
# #   - one
# #   - two
# 
# # --- # Hash containing a list
# # description: balls
# # items:
# #   - red ball
# #   - blue ball
# #   - green ball
# 
# # --- # list containing hashes
# # - name: ball
# #   color: red
# # - name: bat
# #   color: brown
# # - name: glove
# #   color: black
