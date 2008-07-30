:- op(1200, xfx, ==>).
%:- op(1000, xfy, '|').

:- cl('../../config.pl').
:- require('util/util.pl').

==>(LHS,RHS) :-
	nl,write('LHS: '), write(LHS),nl,
	write('RHS: '), write(RHS),nl,
	functor(LHS,F,A),
	write('functor: '), write(F),nl,
	write('Arity: '), write(A),nl,
	LHS =.. [ '|', Head,Conditions],
	write('rule head: '), write(Head),nl,
	write('conditions(unparsed): '), write(Conditions),nl,
	clause_to_list(Conditions,CondList),
	write('conditions list: '), write(CondList),nl.
	

parse_conditions((C,(Rest)),[C|CR]) :-
	parse_conditions(Rest,CR).
parse_conditions(C,[C]).

	
test :-
	(s(X,Y)|value1,value2,value3 ==> body(X,Y)),
	write('-----------------------------------'),nl,
	(s(X,Y)|a ==> body(X,Y)).
	
:- test.