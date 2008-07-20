all_words(Words) :-
	setof(W,sentence_word(W),Words).

all_tags(Tags) :-
	setof(T,sentence_tag(T),Tags).

sentence_tag(T) :-
	sentence(_,Tags),
	member(T,Tags).

sentence_word(W) :-
	sentence(Words,_),
	member(W,Words).
