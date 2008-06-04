require 'brown_corpus.rb'
require 'prolog_helpers.rb'

include Corpus

def report_dups
 corpus = BrownCorpus.new("/usr/share/nltk/data/corpora/brown") 
 corpus.report_on_dups 
 puts corpus.tags.inspect 
end

def test_convert_list
  puts ["a",["b","c","d"],"e"].to_prolog
  puts ["'", "'b","c"].to_prolog
end

def test_brown_to_prolog
  puts BrownCorpus.new("/usr/share/nltk/data/corpora/brown").to_prolog
end

def test_create_descriptions
  BrownCorpus.new(nil).parse_tag_descriptions
end

#test_brown_to_prolog
#test_convert_list
test_create_descriptions