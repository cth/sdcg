% Create list of tags where each tag
% tagwords/2:
% tagword(A,B) - where A is a tag and B is list of words

:- cl('../../util/util.pl').
:- ['focable.pl'].
:- cl('tagmap.pl').

write_tagwords(OutputFile) :-
	current_output(PreviousStream),
	open(OutputFile,write,Stream),
	set_output(Stream),
	listing(tagword),
	set_output(PreviousStream),
	close(Stream).
	
process_sentence([],[]) :- write('.').
process_sentence([Word|WordsRest],[Tag|TagsRest]) :-
	(tagword(Tag,Current) ->
		union(Current,[Word],Words),
		retract(tagword(Tag,_))	
		;
		Words = [Word]
	),
	assert(tagword(Tag,Words)),
	process_sentence(WordsRest,TagsRest).
	
process_all_sentences :-
	sentence(Tags,Words),
	process_sentence(Tags,Words).

create_tagswords :-
	findall(_,process_all_sentences,_).
	
:- 
	assert(tagword(blah,blah)),
	write('creating tag/words mapping'), nl,
	create_tagswords, 
	retract(tagword(blah,blah)),
	write('writing mapping to file...'),nl,
	write_tagwords('tagwords.pl').




	
	
