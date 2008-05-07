%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit tests for the SDCG compiler 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-cl('../compiler/sdcg.pl').

:- test_all.

% Ideally we would build this automatically, but clause(X,_) doesn't work in BProlog :-(
test_clauses([	compact_list, 
				compose_list,
				unifiable_list,
				peel_rightmost,
				update_msw, 
				hyphenate,
				replacement_name ]).

test_all :-
	test_clauses(ClauseList),
	test_run_tests(ClauseList).
	
test_run_tests([]) :- write('Finished running tests'), nl.
test_run_tests([Clause|R]) :-
	atom_codes(Clause,ClauseName),
	append("test_", ClauseName, TestCaseName),
	atom_codes(TestCaseClause, TestCaseName),
	(clause(TestCaseClause,_) ->
		write('Running test: '), write(TestCaseClause), write(' - '),
		(call(TestCaseClause) -> write('OK') ; write('Failed'))
	;
		write('No test defined for clause: '), write(Clause)
	),
	nl,
	test_run_tests(R).

test_compact_list :-
	compact_list([[a],[b],[c],d,[e],[f]],[[a,b,c],d,[e,f]]).

test_compose_list :-
	compose_list((a,(b,c)),[a,b,c]).
	
test_unifiable_list :-
	unifiable_list(0,[]),
	unifiable_list(3,[X,Y,Z]),
	var(X), var(Y), var(Z).
	
test_peel_rightmost :-
	peel_rightmost(a_1_1,[97,95,49],[49]),
	peel_rightmost("a_1_1",[97,95,49],[49]).
	
test_hyphenate :-
	hyphenate(first, second, first_second),
	hyphenate(first_second, third, first_second_third).
	
test_replacement_name :-
	replacement_name(funny_name, 3, 42, funny_name_3_42).

% update_msw should work like queue, last inserted item in front
test_update_msw :-
	retractall(values(_,_)), % setup
	update_msw(test, first_element),
	values(test,[first_element]),
	update_msw(test, [next,next_next]),
	values(test,[[next,next_next],first_element]),
	retractall(values(_,_)). % teardown
	
test_expand_values :-
	retractall(values(_,_)),
	expand_values(test,1,test_1_1),
	expand_values(test,1,test_1_2),
	expand_values(test,3,test_3_1),
	retractall(values(_,_)).

