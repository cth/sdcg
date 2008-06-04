sdcg(TagList) ==> tag(none,_,TagList).

%tag(none,[Tag|TagsRest]) ==>
%	@word_l(W),
%	tag(Tag,TagsRest).

@brown_tag ==> 
	@word_l(W).

tag(@brown_tag(Tag),@brown_tag(Next), [Tag|TagsRest]) ==>
	@word_l(W),
	tag(NextTag,TagsRest).
	
% Matching the last word of a sentence.
tag(@brown_tag(T),_,[T|[]]) ==>
	@word_l(W).

%stop(@brown_tag,[]) ==> [].