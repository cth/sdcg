%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Attempt to parse the brown corpus
% Assumes that generated_sdcg.psm exist
% Load compiler
:- cl('../../sdcg.pl').
:- include_rel('grammars/english/brownlex/sentences.pl').
:- load_grammar.
%:- findall(_,parse_sentences,_).
:- create_parseable_sentences_file('psentences.pl').

load_grammar :-
	prismn(generated_sdcg).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create a file with parsable sentences
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
write_parseable_sentence(Words,Tags) :-
	(prob(start(Tags,[]),P) ->
		portray_clause(sentence(Words,Tags))
		;
		true
	).

write_parseable_sentences :-
	sentence(Tags,Words),
	write_parseable_sentence(Tags,Words).

create_parseable_sentences_file(File) :-
	write('commencing parse of all sentences'),nl,
	open(File, write, Stream),
	current_output(PreviousStream),
	set_output(Stream),	
	findall(_,write_parseable_sentences,_),
	set_output(PreviousStream),
	close(Stream).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
parse_sentence(Words,Tags) :-
	write('attempting to parse sentence: '),
	write(Tags),
	write(' -- '), write(Words),
	write(' - '),
	(prob(start(Tags,[]),P) ->
		write('probability: '),
		write(P)
		;
		write('failed')
	),
	nl.

parse_sentence_ap(Words,Tags) :-
	write('attempting to parse sentence: '),
	write(Tags),
	write(' - '),
	(prob(start_append(Tags),P) ->
		write('probability: '),
		write(P)
		;
		write('failed')
	),
	nl.
	

	
% Fails if the sentence cannot be parsed
test_parse_sentence(Words,Tags) :-
	prob(start(Tags,[]),_).
test_parse_sentence_append(Words,Tags) :-
	prob(start(Tags),_).
	
%count_parseable_sentences :-
	
parse_sentences :-
	sentence(Tags,Words),
	length(Words,L),
	L < 6,
	parse_sentence(Tags,Words).

