%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some helper functions for dealing with prolog formatted brown corpus.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Count number of tags
count_tags(Count) :-
	setof(X,brown_tag(X),L),
	length(L,Count).

% Sentence is the sentence with SentenceNumber but with tags removed
remove_tags(SentenceNumber,Sentence) :-
	brown_sentence(SentenceNumber,WordsTags),
	sentence_words(WordsTags,Sentence).

show_tag_examples(Tag, NumExamples) :-
	setof(N,all_sentences_tagged(Tag,N),SentenceNumbers),
	show_sentences_with_tags(SentenceNumbers,NumExamples).

show_tag_words(Tag) :-
	setof(X,brown_pos_tag(X,Tag),Words),
	write_words(Words).

all_sentences_tagged(Tag,Number) :-
	brown_sentence(Number,WordsTags),
	sentence_is_tagged(Tag,WordsTags).

sentence_is_tagged(Tag,[[_,Tag]|_]).
sentence_is_tagged(Tag,[[_,OtherTag]|R]) :-
	Tag \== OtherTag,
	sentence_is_tagged(Tag,R).

show_sentences([], _).
show_sentences(_,0).
show_sentences([Number|Rest],X) :-
	show_sentence(Number), nl,nl,
	Y is X - 1,
	show_sentences(Rest,Y).

show_sentence(Number) :-
	remove_tags(Number,Sentence),
	write_words(Sentence).
	
show_sentences_with_tags([], _).
show_sentences_with_tags(_,0).
show_sentences_with_tags([Number|Rest],X) :-
	show_sentence_with_tags(Number), nl,nl,
	Y is X - 1,
	show_sentences_with_tags(Rest,Y).	
	
show_sentence_with_tags(Number) :-
	brown_sentence(Number,WordsTags),
	write_words_tags(WordsTags).

sentence_words([],[]).
sentence_words([[Word,_]|SentenceRest], [Word|WRest]) :-
	sentence_words(SentenceRest,WRest).
	
write_words([]).
write_words([Word|R]) :-
	write(Word),
	write(' '),
	write_words(R).

write_tagged_tags([]).
write_words_tags([[Word,Tag]|Rest]) :-
	write(Word),
	write('/'),
	write(Tag),
	write(' '),
	write_words_tags(Rest).
