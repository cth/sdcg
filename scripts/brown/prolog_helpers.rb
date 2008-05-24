  class String
  def to_prolog
    "'" + self.gsub("'",'\\\\\'') + "'"
  end
end

class Array
  def to_prolog
    "[" + (self.map { |e| e.to_prolog}).join(',') + "]"
  end
end