words = []
File.open('focable.pl') do |file|
	file.each do |line|
		/word\(([\.:\$,-_a-z\d]*)\)/.match(line).captures.each do |word|
			words << word	
		end
	end
end

words.sort.uniq.each do |word|
	next if word =~ /_/
	next if word =~ /A/
	puts "word(#{word})."
end
