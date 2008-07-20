% Since the Brown corpus isn't first order compilable, we need to preprocess it.
% This is done by replacing quotes with an other "recognizeable" symbol
% Since, the word "uquot" doesn't appear in corpus, so all quotes are replaced with this symbol.

%:- cl('brown_parts').
:- cl('brown_helper').
:- cl('../../util/util.pl').
:- cl('tagmap.pl').
:- ['tmp1.pl'].

wordmap(',',comma_).
wordmap('-',hyphen_).
wordmap('--',hyphen_).
wordmap('(',start_paran_).
wordmap(')',end_paran_).
wordmap('.',dot_).
wordmap(':',colon_).
wordmap(';',semicolon_).

create_foc_version(OutputFile) :-
	current_output(PreviousStream),
	open(OutputFile,write,Stream),
	set_output(Stream),
	process,
	set_output(PreviousStream),
	close(Stream).

process :-
	findall(Sentence,brown_sentence(_,Sentence),Sentences),
	process_sentences(Sentences).
	
process_sentences([]).
process_sentences([Sentence|Rest]) :-
	process_sentence(Sentence),
	process_sentences(Rest).
	
process_sentence(Sentence) :-
	write('% process sentence: '), write(Sentence),nl,
	sentence_words(Sentence,Words),
	process_words(Words,NewWords),
	sentence_tags(Sentence,Tags),
	(process_tags(Tags,NewTags) ->
		write(sentence(NewWords,NewTags))
		;
		write('%Removed% '),write(sentence(Words,Tags))
	),
	write('.'),
	nl.
	
process_tags([],[]).
process_tags([Tag|TRest],[NewTag|NewTRest]) :-
	(tagmap(Tag,NewTag) -> true ; write('% no tagmap for'), write(Tag),nl),
	process_tags(TRest,NewTRest).

process_words([],[]).
process_words([Word|Rest],[ProcessedWord|Rest2]) :-
	process_word(Word,ProcessedWord),
	process_words(Rest,Rest2).
	
process_word(Word,word(ProcessedWord)) :-
	wordmap(Word,ProcessedWord).
process_word(Word,word(Word)).
	
%process_word(Word,ProcessedWord) :-
%	atom_codes(Word,WordAsList),
%	list_replace("'","_quote_",WordAsList,ProcessedWordAsList),

:- create_foc_version('tmp2.pl').
