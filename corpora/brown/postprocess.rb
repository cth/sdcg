File.open('tmp2.pl') do |file|
	file.each do |line|
		next if line =~ /^%.*/ # Skip comment lines
		
		line = line.gsub(/word\(([\.:\$,-_a-z\d]*)\)/) do |word|
			word.gsub!(/^\./,"dot_")
			word.gsub!(/\./,"_dot_")
			word.gsub!(/^\$/,"dollar_")
			word.gsub!(/\$/,"_dollar_")
			word.gsub!(/^:/,"colon_")
			word.gsub!(/:/,"_colon_")
			word.gsub!(/^,/,"comma_")
			word.gsub!(/,/,"_comma_")
			word = word.gsub(/^(\d+)/) do |d|
			  "num_#{d}"
		  end
			word
		end
		puts line
	end
end
