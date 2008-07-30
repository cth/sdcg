load_dynamic :- true.
:- 	['../../sdcg.pl'].

test :-
	LHS =.. [ rule, @tag(A), [A|B] ],
	RHS = (@tag(B)),
	Rule =.. [ ==>, LHS, RHS ],
	portray_clause(Rule),
	create_expansion_rule(LHS,RHS,ExpRule),
	portray_clause(ExpRule).
	
test2 :-
	create_implementation_rule(tag_word_3_1,tag_word,[adjective,adjective,[adjective|Rest]],[[jumped],sdcg_regex_maybe_tag_word(adjective,_,Rest)],Rule),
	portray_clause(Rule).
	
%	create_expansion_rule
