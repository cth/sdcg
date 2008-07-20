words = []
File.open('focable.pl') do |file|
	file.each do |line|
		/word\(([\.:\$,-_a-z\d]*)\)/.match(line).captures.each do |word|
			words << word	
		end
	end
end

words.sort.uniq.each do |word|
	puts "word(#{word})."
end
