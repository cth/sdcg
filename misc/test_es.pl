expand_asserted_set(Name, NewValue) :-
	Clause =.. [Name, Values],
	% The set clause might not have been asserted yet
	catch(Clause, _, Values = []),
	((Values == []) -> 
		NewClause =.. [ Name, [NewValue]]
		;
		retract(Clause),
		union([NewValue],Values,NewValues),
		NewClause =.. [ Name, NewValues ]
	),
	assert(NewClause).

test :-
	expand_asserted_set(rules,(a(D, E, F, A) :- incr_depth(A, B), msw(a(1), C), sdcg_rule(C, D, E, F, B))),
	expand_asserted_set(rules,(b(D, E, F, A) :- incr_depth(A, B), msw(a(1), C), sdcg_rule(C, D, E, F, B))),
	expand_asserted_set(rules,(c(D, E, F, A) :- incr_depth(A, B), msw(a(1), C), sdcg_rule(C, D, E, F, B))),
	rules(X),
	write(X).
