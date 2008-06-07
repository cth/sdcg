#!/usr/bin/env ruby
# Delete generated temporary files
# Christian Theil Have, 2008.

TMP_FILE_PATTERN = /(.*\.out)|(temp.*)/

def remove_tmp(dir)
  puts "Processing dir " + dir.to_s
  Dir.new(dir).each do |file|
    next if file =~ /^\./
    fullpath = dir + "/" + file
    if File.directory?(fullpath)
      remove_tmp(fullpath)
    else
      if file =~ TMP_FILE_PATTERN
        puts "deleting #{fullpath}"
        File.delete fullpath if file =~ TMP_FILE_PATTERN        
      end
    end
  end
end

remove_tmp(Dir.pwd)

puts "Done."
