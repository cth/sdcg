% How to do this:
% - Each sentence must have some sort of stop marker. 
% - Or maxdepth must be set to the maximal sentence length.

consume_word([Word]) :-
	word(Word).
	
conditioning_mode(tag_word(+,-,-)).

% Taglist contains a list of tags after application of the start rule
start(TagList) ==> 
	tag_word(none,_,TagList).
	
% Feature 1: The previous tag
% Feature 2: The current tag
% Feature 3: A list of tags encountered so far
/*
Won't train
tag_word(Previous, @tag(Current), [Current|TagsRest]) | @tag(Previous) ==>
	@consume_word(W),
	?(tag_word(Current,_,TagsRest)).
*/
tag_word(Previous, @tag(Current), [Current|TagsRest]) | @tag(SomeTag) ==>
	@consume_word(W),
	tag_word(Current,_,TagsRest).
	
tag_word(Previous, @tag(Current), [Current]) | @tag(SomeTag) ==>
	@consume_word(W).