% We probabily need a lexicalized model for this.

:- cl('../compiler/sdcg.pl').

generate_samples(N,Gs):-
        get_samples_c([inf,N],s(_,_,[]),true,Gs).

/*
parse_tree_to_sentence(T,S) :-
	functor(T,Word,0),
	S = [ Word ].
parse_tree_to_sentence(T,S) :-
	functor()
	T =.. [ No]
*/

run :-
	sdcg_set_option(start_symbol,s),
	sdcg_set_option(debug,yes),
	sdcg('sdcg_grammar.pl').

correct_sentence([this,sentence,is,faulty]).
incorrect_sentence([this,sentence,sentence,is,faulty]).

test :-
	correct_sentence(Correct),
	incorrect_sentence(Incorrect),
	report_on(Correct),
	report_on(Incorrect).
	
	
report_on(Sentence) :-
	write('Sentence: '), write(Sentence),nl,
	viterbig(s(T1,Sentence,[])),
	prob(s(T1,Sentence,[]),P1),
	write('Parse tree: '), write(T1),nl,
	write('Probability: '), write(P1),nl.