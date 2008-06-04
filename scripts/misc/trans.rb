def trans_list(str, pred)
	str.gsub!(/ /, "\n")
	str.gsub!(/(.*)\n/, "#{pred}($1).")
	str
end
