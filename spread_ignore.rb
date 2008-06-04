# Spread the toplevel .gitignore into all subdirs
# Christian Theil Have, 2008.

IGNORE_FILE=".gitignore"

def spread_ignore(dir,ignore)
	puts "Processing dir " + dir.to_s
	Dir.new(dir).each do |file|
		next if file =~ /^\./
		fullpath = dir + "/" + file
		spread_ignore(fullpath,ignore) if File.directory?(fullpath)
	end
	File.open(dir+"/"+IGNORE_FILE,"w") { |file| file << ignore }
end

contents = *open(IGNORE_FILE)
spread_ignore(Dir.pwd,contents.join)
puts "Done."
