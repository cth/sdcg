%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit tests for the SDCG compiler 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-cl('../compiler/sdcg.pl'). % Why doesn't load operator definition from this??
:- op(1200, xfx, ==>).
:- op(1200, xfx, @=>).

:- run.
		
run :-
	nl, write('===== Runnning all testcases ====='),nl,
	open('test.pl', read, Stream),
	ground(Stream),
	read_rules(Stream, Rules),
	run_test_rules(Rules),
	close(Stream).

run_test_rules([]).
run_test_rules([Rule|Rest]) :-
	((Rule =.. [ :-, Head, _ ]) ->
		(functor(Head, F, 0) ->
			atom_codes(F,FList),
			(append("test_", _, FList) -> 
				write(F), write(' - '),
				(call(Head) -> write('OK') ; write('Failed')),
				nl
				; true)	; true) ; true),
	run_test_rules(Rest).

test_compact_list :-
	compact_list([[a],[b],[c],d,[e],[f]],[[a,b,c],d,[e,f]]).

test_clause_to_list :-
	clause_to_list((a,(b,c)),[a,b,c]).
	
test_atom_append :-
	atom_append(abc,def,abcdef).
	
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
	
test_expand_msw :-
	retractall(values(_,_)),
	expand_msw(test,1,test_1_1),
	expand_msw(test,1,test_1_2),
	expand_msw(test,3,test_3_1),
	retractall(values(_,_)).
	
test_combine_two :-
	combine_two([[a,a],[b,b],[c,c]],[[x,x],[y,y],[z,z]],
	[[a,a,x,x],[b,b,x,x],[c,c,x,x],[a,a,y,y],[b,b,y,y],[c,c,y,y],[a,a,z,z],[b,b,z,z],[c,c,z,z]]).

test_combine :-
	combine([[[a,a]],[[b,b],[b2,b2]],[[c,c]]], [[a,a,b,b,c,c],[a,a,b2,b2,c,c]]).
	combine([[[a,a]],[[b,b],[b2,b2]],[[c,c]],[[d],[e]]],
	[[a,a,b,b,c,c,d],[a,a,b2,b2,c,c,d],[a,a,b,b,c,c,e],[a,a,b2,b2,c,c,e]]).

test_add_functor :-
	add_functor(f, [[a,b,c],[x,y,z]], [f(a,b,c), f(x,y,z)]).

test_remove_ground :-
	remove_ground([a,b,c,d], []),
	remove_ground([X,y], [X]),
	remove_ground([y,X], [X]),
	remove_ground([X,y,Z],[X,Z]).
	
test_regex_rule_head :-
	regex_rule_head(star,test(a,b,c),sdcg_regex_star_test(a,b,c)).
	
test_regex_rule :-
	regex_rule_none(_,test,(test@=>[])),
	regex_rule_one(test,newtest,(newtest@=>test)),
	regex_rule_kleene(_,test,(test@=>test,test)).

test_generate_regex_rules :-
	generate_regex_rules(star, test(a,X,b),NewC, [regex_rule_none, regex_rule_one, regex_rule_kleene], Rules),
	NewC == (sdcg_regex_star_test(a,X,b)),
	Rules == [ 
		(sdcg_regex_star_test(a,X,b)@=>[]),
		(sdcg_regex_star_test(a,X,b)@=>test(a,X,b)),
		(sdcg_regex_star_test(a,X,b)@=>sdcg_regex_star_test(a,X,b),sdcg_regex_star_test(a,X,b))
	].

test_expand_constituent :-
	% Test star operator:
	expand_constituent(*(a(b,V1)),NewC1,Rules1),
	NewC1 = (sdcg_regex_star_a(b,V1)),
	Rules1 == [	
		(sdcg_regex_star_a(b,V1)@=>[]),
		(sdcg_regex_star_a(b,V1)@=>a(b,V1)),
		(sdcg_regex_star_a(b,V1)@=>sdcg_regex_star_a(b,V1),sdcg_regex_star_a(b,V1))
	].
	
test_resolve_expand_mode :-
	% setup 
	assert(number(he,sg)),
	assert(expand_mode(number(-,+))),
	% test:
	Expander =.. [ number, X, Y ],
	resolve_expand_mode(Expander, ModeList),
	%       write(ModeList),nl,
	ModeList == [-,+], % correctness assertion
	% cleanup
	retract(number(he,sg)),
	retract(expand_mode(number(-,+))).

test_arg_expand_list :-
        % setup 
        assert(word(he,sg,masc)),
        assert(expand_mode(word(-,+,+))),
        % test:
        Args = [ a,b,c ],
        Expander =.. [ word | Args ],
        resolve_expand_mode(Expander, ModeList),
        arg_expand_list(Args, ModeList,NewArgList),
		NewArgList == [b,c],
        % cleanup
        retract(word(he,sg,masc)),
        retract(expand_mode(word(-,+,+))).

test_expand_asserted_set :-
	expand_asserted_set(myset,a1),
	expand_asserted_set(myset,a2),
	expand_asserted_set(myset,a2),
	expand_asserted_set(myset,a3),
	myset(Set),
	set_equal(Set,[a1,a2,a3]),
	retract(myset(Set)).