require 'prolog_helpers.rb'

module Corpus
  class GenericCorpus
  end
  
  # Used to create a prolog lexicon from a set of 
  class BrownCorpus < GenericCorpus
    
    def initialize(dir, skip_files = [ "README", "CONTENTS"] )
      @dir = dir
      @sentences = []

      Dir.new(dir).each do |file|
        parse @dir + "/" + file unless skip_files.include? file or file =~ /^\..*/
      end

      puts "total sentences parsed: " + @sentences.size.to_s
      puts "total unique word POS tags: " + tags.size.to_s
    end

    def to_prolog_lists
      @sentences.each do |s|
        puts s.inspect
      end
    end
    
    # Iterator over all elements (word/tag/in the corpus
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
      sentence.split(" ").each { |wt|	s << wt.split("/") }
      s
    end
    
    # Generates prolog, for easy access of the brown corpus
    # Each unique word/tag combination is asserted as brown_pos_tag(Word,Tag).
    # Each sentence is asserted as brown_sentence([[Word1,Tag1],[Word2,Tag2], ...]).
    def to_prolog
      prolog_clauses = []
      @sentences.each do |s|
        prolog_clauses << "brown_sentence(" + s.to_prolog + ").\n"
        s.each do |e|
          tags["brown_pos_tag("+ e[0].to_prolog + ',' + e[1].to_prolog + ")."] = true
        end
      end
      tags.map { |t,| prolog_clauses << t if t =~ /^brown_pos_tag/ }
      prolog_clauses.join("\n")
    end
  end
end