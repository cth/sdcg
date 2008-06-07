:- cl('../compiler/sdcg.pl').
:- sdcg_set_option(debug,yes).
:- sdcg_set_option(prism_invoker,prism).
:- sdcg('../grammars/english.pl').

generate_samples(N,Gs):-
        get_samples_c([inf,N],sdcg(X,[]),true,Gs).

test :-
	write('Attempting to generate 1000 sample sentences'),nl,
	generate_samples(1000,S),nl,
	write('Checking the probability of the samples:'),nl,
	check_samples(S).

makemany :-
	generate_samples(100, X),
	write(X).

check_samples([]).
check_samples([Sample|Rest]) :-
	write(Sample), 
	write(' -- probability='),
	prob(Sample, P),
	write(P),
	write(' -- viterbi probability='),
	viterbi(Sample,PV),
	write(PV),
%	((P >= PV) -> write(' OK') ; (write(' ERROR!')),
	nl,
	check_samples(Rest).