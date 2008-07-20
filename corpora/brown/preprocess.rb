File.open('brown_corpus_whole.pl') do |file|
	file.each do |line|
		line.downcase!
		line.gsub!(",['``','``']",",[startquote,startquote]")
		line.gsub!("['``','``'],","[startquote,startquote],")
		line.gsub!("['``','``']","[startquote,startquote]")
		line.gsub!(",['\\'\\'','\\'\'']",",[endquote,endquote]")
		line.gsub!("['\\'\\'','\\'\''],","[endquote,endquote],")
		line.gsub!("['\\'\\'','\\'\'']","[endquote,endquote]")
		line.gsub!("\\'","_quote_")
		line.gsub!("['_quote__quote_','_quote__quote_']","[endquote,endquote]")
		puts line
	end
end
