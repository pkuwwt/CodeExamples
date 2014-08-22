#!/usr/local/bin/ruby

##
# @file      thread01.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2006 by Mitch Richling.  All rights reserved.
# @brief     Simple thread creation in Ruby.@EOL
# @Keywords  ruby example thread
# @Std       Ruby 1.8

Thread.new() {           (1..5).each {|i| printf("THREAD0 %d\n", i);} }

Thread.new() { sleep(1); (1..5).each {|i| printf("THREAD1 %d\n", i); sleep(1);} }

Thread.new() { sleep(2); (1..5).each {|i| printf("THREAD2 %d\n", i); sleep(1);} }

puts("All threads started...")

# Join each thread except this one so they all finish before we go on
Thread.list.each {|t| puts(t); t.join() unless (t == Thread.current) }

puts("All threads finished...")
