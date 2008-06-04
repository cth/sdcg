require 'rubygems'
require 'hpricot'
require 'open-uri'

def parse_tag_descriptions
	doc = Hpricot(open('http://www.scs.leeds.ac.uk/ccalas/tagsets/brown.html'))
  tag_descriptions = []
	(doc/"html"/"table"/"tr").each do |tr|
	  idx=1
	  tag = description = examples = nil
	  (tr/"td").each do |d|
	    text = d.inner_text.strip.tr("\n", ' ')
	    case idx
      when 1; tag = text.downcase
      when 2; description = text
      when 3; examples = text.gsub(/\s+/, ' ')
      end
	    idx += 1
    end
    tag_descriptions << [tag, description, examples ]
	end
	
	#puts tag_descriptions.inspect
	
	clauses = []
  tag_descriptions.map do |td|
    clauses << "brown_tag_description" + (td.map! { |e| e.to_prolog }).join(",") + ")."
#    clauses <<  "brown_tag_description(" + td[0].to_prolog + "," +  td[1].to_prolog + "," + t[2].<to></to>
#                v[0].to_prolog + ","
  end
 puts clauses
end

parse_tag_descriptions
