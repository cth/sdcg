require 'rubygems'
require 'hpricot'
require 'open-uri'

require 'prolog_helpers.rb'

module Corpus
  class GenericCorpus
  end
  
  # Used to create a prolog lexicon from a set of 
  class BrownCorpus < GenericCorpus
    
    def initialize(dir = nil)
      @sentences = []      
      load(dir) unless dir.nil?
    end
    
    def load(dir, skip_files = [ "README", "CONTENTS"] )
      @dir = dir

      Dir.new(dir).each do |file|
        parse @dir + "/" + file unless skip_files.include? file or file =~ /^\.+/
      end

      puts "total sentences processed: " + @sentences.size.to_s
      puts "total unique word POS tags: " + tags.size.to_s + " -- Here they are:"
      #tags.map { |t,| print t.to_s + " " }
    end

    # Iterator over all elements (word/tag) in the corpus
    def each_element
      @sentences.each { |s| s.each { |e| yield e } }
    end
    
    # Report on words that occur twice in sequence
    def report_on_dups
      @sentences.each do |s|
        # Count pairs
        previous = ""
        s.each do |word,tag|
          if word == previous and tag != "." and tag != ".-hl" and tag != ","
            puts word + " " + tag
            puts (s.map { |e| e.first }).join(' ')
          end
          previous = word
        end
      end
    end

    # Creates a Hashmap contain the POS tags of the corpus as keys
    # and the number of occurences in the corpus as values
    def tags
      if @tags.nil? # build categories
        @tags = {}
        @sentences.each do |sentence|
          sentence.each do |element|
            if @tags[element[1]].nil?
              @tags[element[1]] = 1
            else
              @tags[element[1]] = @tags[element[1]] + 1
            end
          end
        end
      end

      @tags
    end

    # Parse a corpus file
    def parse(file)
      File.open(file).each_line do |line|
        line = line.strip
        @sentences << parse_sentence(line) unless line.empty?
      end
    end

    # Parse a sentence
    def parse_sentence(sentence)
      s = []
      sentence.gsub(/\/\//,"__slash__/").split(" ").each do |wt|
        tmp = wt.split "/"
        # Some sentence may contain words with slashes (milliamperes/cell/nns). Fix this:
        tmp = "#{tmp[0]}/#{tmp[1]}","#{tmp[2]}" while tmp.size > 2
        #tmp[1].gsub!(/__slash__/,"/")
        tmp = [ tmp[0], "/" ] if tmp[1] == "__slash__"
        
        # Remove headline -nc, -tl, -hl hyphenations
        tmp[1] = tmp[1].gsub(/-nc/,"").gsub(/-tl/,"").gsub(/-hl/,"")
        s << tmp
      end
      s
    end
    
    # Generates prolog, for easy access of the brown corpus
    # Each unique word/tag combination is asserted as brown_pos_tag(Word,Tag).
    # Each sentence is asserted as brown_sentence([[Word1,Tag1],[Word2,Tag2], ...]).
    def to_prolog
      prolog_clauses = []
      words = {} 
      sentence_number = 1

      prolog_clauses << "%% Unique tags in the corpus:\n"
      tags.map { |t,| prolog_clauses << "brown_tag(#{t.to_prolog})." }

      @sentences.each do |s|
        prolog_clauses << "brown_sentence(#{sentence_number},#{s.to_prolog}).\n"
        s.each { |e| words["brown_word(#{e[0].to_prolog})."] = true }
        sentence_number += 1
      end

      prolog_clauses << "%% Unique words in the corpus"
      words.map { |w,| prolog_clauses << w }

      (parse_tag_descriptions + prolog_clauses).join("\n")
    end

    # Parse tag descriptions from the web-page: http://www.scs.leeds.ac.uk/ccalas/tagsets/brown.html
    def parse_tag_descriptions
    	doc = Hpricot(open('http://www.scs.leeds.ac.uk/ccalas/tagsets/brown.html'))
      tag_descriptions = []
      tag_keywords = {}
    	(doc/"html"/"table"/"tr").each do |tr|
    	  idx=1
    	  tag = description = examples = nil
    	  (tr/"td").each do |d|
    	    text = d.inner_text.strip.tr("\n", ' ').gsub(/\s+/, ' ')
    	    case idx
          when 1; tag = text.downcase
          when 2; description = text
          when 3; examples = text
          end
    	    idx += 1
        end
        tag_descriptions << [ tag, description, examples ] unless tag.nil? or tag == "Tag"
        # Use the descriptions to pull out some search keywords for the tag
        if not tag.nil? and description.nil?
          puts "Description for " + tag + " is nil!!!"
        end
        tag_keywords[tag] = description.tr(",",'').tr("+",'').split(/ /) unless tag.nil? or tag == "Tag"
    	end
    	
    	clauses = []
      tag_descriptions.map do |td|
        clauses << "brown_tag_description(" + (td.map { |e| e.to_prolog }).join(",") + ")."
      end

      tag_descriptions.map do |td|
        clauses << "brown_tag_description(" + (td.map { |e| e.to_prolog }).join(",") + ")."
      end
      tag_keywords.each_key do |key|
        tag_keywords[key].each do |keyword|
          clauses << "brown_tag_keyword(" + key.to_prolog + "," + keyword.to_prolog + ")."
        end
      end
      clauses
    end
  end
end
