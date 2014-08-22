#!/usr/local/bin/ruby

##
# @file      fileUtils.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2007 by Mitch Richling.  All rights reserved.
# @brief     Demonstrate the fileUtils module.@EOL
# @Keywords  ruby example
# @Std       Ruby 1.8
#
#            fileutils is an awesome module providing one stop
#            shopping for a broad range of shell-like file and
#            filesystem manipulations methods -- most methods even
#            take option strings that mimic the command line tools the
#            methods reproduce.  Some of the methods provided:
#
#                        cd(dir, options)
#                        cd(dir, options) {|dir| .... }
#                        pwd()
#                        mkdir(dir, options)
#                        mkdir(list, options)
#                        mkdir_p(dir, options)
#                        mkdir_p(list, options)
#                        rmdir(dir, options)
#                        rmdir(list, options)
#                        ln(old, new, options)
#                        ln(list, destdir, options)
#                        ln_s(old, new, options)
#                        ln_s(list, destdir, options)
#                        ln_sf(src, dest, options)
#                        cp(src, dest, options)
#                        cp(list, dir, options)
#                        cp_r(src, dest, options)
#                        cp_r(list, dir, options)
#                        mv(src, dest, options)
#                        mv(list, dir, options)
#                        rm(list, options)
#                        rm_r(list, options)
#                        rm_rf(list, options)
#                        install(src, dest, mode = <src's>, options)
#                        chmod(mode, list, options)
#                        chmod_R(mode, list, options)
#                        chown(user, group, list, options)
#                        chown_R(user, group, list, options)
#                        touch(list, options)
#                        copy_entry(src, dest, preserve = false, dereference = false)
#                        copy_file(src, dest, preserve = false, dereference = true)
#                        copy_stream(srcstream, deststream)
#                        remove_entry(path, force = false)
#                        remove_entry_secure(path, force = false)
#                        remove_file(path, force = false)
#                        compare_file(path_a, path_b)
#                        compare_stream(stream_a, stream_b)
#                        uptodate?(file, cmp_list)

require 'fileutils' 

FileUtils.cd('.')

puts("CWD: #{FileUtils.pwd}")
