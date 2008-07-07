%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit tests for the SDCG compiler 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- cl('../sdcg.pl').
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
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Test of various utility rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	replacement_name(funny_name, 3, 42, [], funny_name_3_42).

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
	
test_retract_each :-
	A = blah(blah,blah),
	B = foo(bar),
	C = answer(42),
	assert(A),assert(B),assert(C),
	retract_each([A,B,C]),
	not clause(A,_),
	not clause(B,_),
	not clause(C,_).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MSW and rule assertions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
test_expand_asserted_set :-
	expand_asserted_set(myset,a1),
	expand_asserted_set(myset,a2),
	expand_asserted_set(myset,a2),
	expand_asserted_set(myset,a3),
	myset(Set),
	set_equal(Set,[a1,a2,a3]),
	retract(myset(Set)).
	
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Test of regular expression expansion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Test macro expansion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

test_resolve_expand_mode1 :-
	not resolve_expand_mode(blah(x,_,z),_),
	not resolve_expand_mode(blah(_),_),
	assert(expand_mode(blah(+,+,-))),
	resolve_expand_mode(blah(_X,_Y,_Z), [+,+,-]),
	resolve_expand_mode(blah(x,y,z), [+,+,-]),
	retract(expand_mode(blah(+,+,-))).

test_resolve_expand_mode2 :-
	% setup 
	assert(number(he,sg)),
	assert(expand_mode(number(-,+))),
	% test:
	Expander =.. [ number, X, Y ],
	resolve_expand_mode(Expander, ModeList),
	ModeList == [-,+], % correctness assertion
	% cleanup
	retract(number(he,sg)),
	retract(expand_mode(number(-,+))).

test_expand_feature :-
	% We expect ground variables to be removed since there is no expand_mode pattern
	expand_feature(@blah(a,A,B,c,d),Expander1,[A,B]), 
	Expander1==blah(a,A,B,c,d),	
	assert(expand_mode(blah(-,+,-,-,+))),
	expand_feature(@blah(a,X,Y,c,d),Expander2,[X,d]),
	Expander2==blah(a,X,Y,c,d),
	retract(expand_mode(blah(-,+,-,-,+))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Compiler options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_default_options :-
	sdcg_option(start_symbol, sdcg),
	sdcg_option(parsetree,false),
	sdcg_option(maxdepth,0),
	sdcg_option(prism_file, 'generated_sdcg.psm'),
	sdcg_option(prism_invoker, prismn),
	sdcg_option(debug,false).
	
test_set_unset_option :-
	sdcg_set_option(parsetree,true),
	sdcg_option(parsetree,true),
	sdcg_unset_option(parsetree),
	sdcg_option(parsetree,false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Compiler rule creation / rewriting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
% Test rewrite with just difference rules. Default behaviour.
test_rewrite_rule_rhs_simple :-
	rewrite_rule_rhs(In,Out,Depth,[const1(a,b),const2(c,d)],Parsetree,(const1(a,b,In,Out1),const2(c,d,Out1,Out))).
	
test_rewrite_rule_rhs_with_parsetree_1 :-
	sdcg_set_option(parsetree),
	rewrite_rule_rhs(In,Out,_Depth,
		[const1(a,b)], % Input constitutents
		[[const1(a,b),ParseTreeChild1]], % Expected parsetree
		(const1(a,b,In,Out1,ParseTreeChild1))
	),
	sdcg_unset_option(parsetree).

test_rewrite_rule_rhs_with_parsetree_2 :-
	sdcg_set_option(parsetree),
	rewrite_rule_rhs(In,Out,_Depth,
		[const1(a,b),const2(c,d)], % Input constitutents
		[[const1(a,b),ParseTreeChild1],[const2(c,d),ParseTreeChild2]], % Expected parsetree
		(const1(a,b,In,Out1,ParseTreeChild1),(const2(c,d,Out1,Out2,ParseTreeChild2)))
	),
	sdcg_unset_option(parsetree).

test_rewrite_rule_rhs_with_parsetree_3 :-
	sdcg_set_option(parsetree),
	rewrite_rule_rhs(In,Out,_Depth,
		[const1(a,b),const2(c,d),const3(e,f),const4(g,h)], % Input constitutents
		[[const1(a,b),ParseTreeChild1],[const2(c,d),ParseTreeChild2],[const3(e,f),ParseTreeChild3],[const4(g,h),ParseTreeChild4]], % Expected parsetree
		(const1(a,b,In,Out1,ParseTreeChild1),(const2(c,d,Out1,Out2,ParseTreeChild2),(const3(e,f,Out2,Out3,ParseTreeChild3),const4(g,h,Out3,Out,ParseTreeChild4))))
	),
	sdcg_unset_option(parsetree).
	nl,write(Parsetree),nl.
	
% should generate a simple rule containing
test_create_selector_rule :-
	create_selector_rule(testrule,2,[],SelectorRule),
	SelectorRule =.. [ :-, testrule(A,B,C,D), (msw(testrule(2),E),sdcg_rule(E,A,B,C,D)) ].

% This should generate an implementation rule without parse tree and depth feature
test_create_implementation_rule_simple :-
	% Make sure that parse tree and depth options are unset
	sdcg_unset_option(parsetree),
	sdcg_unset_option(maxdepth),
	create_implementation_rule(testrule,[A,b,C],[child(A),child(B),child(C)],ImplRule),
	ImplRule =.. [	:-,
					sdcg_rule(testrule,A,b,C,In,Out),
					(child(A,In,Out1),(child(b,Out1,Out2),child(C,Out2,Out))) ].
					
test_create_implementation_rule_with_parsetree :-
	% Make sure that parse tree and depth options are unset
	sdcg_set_option(parsetree),
	create_implementation_rule(testrule,[A,b,C],[child(A),child(B),child(C)],ImplRule),
	ImplRule =.. [	:-,
					sdcg_rule(testrule,A,b,C,In,Out,[testrule(A,b,C), [Child1Parsetree,Child2Parsetree,Child3Parsetree]]),
					(child(A,In,Out1,Child1Parsetree),(child(b,Out1,Out2,Child2Parsetree),child(C,Out2,Out,Child3Parsetree))) ],
	sdcg_unset_option(parsetree).

test_create_implementation_rule_with_parsetree_include_difflists :-
	% Make sure that parse tree and depth options are unset
	sdcg_set_option(parsetree),
	sdcg_set_option(parsetree_include_difflists),
	create_implementation_rule(testrule,[A,b,C],[child(A),child(B),child(C)],ImplRule),
	ImplRule =.. [	:-,
					sdcg_rule(testrule,A,b,C,In,Out,[testrule(A,b,C,In,Out), [Child1Parsetree,Child2Parsetree,Child3Parsetree]]),
					(child(A,In,Out1,Child1Parsetree),(child(b,Out1,Out2,Child2Parsetree),child(C,Out2,Out,Child3Parsetree))) ],
	sdcg_unset_option(parsetree_include_difflists),
	sdcg_unset_option(parsetree).

test_create_implementation_rule_with_depth :-
	% Make sure that parse tree and depth options are unset
	sdcg_set_option(maxdepth,10),
	create_implementation_rule(testrule,[A,b,C],[child(A),child(B),child(C)],ImplRule),
	ImplRule =.. [	:-,
					sdcg_rule(testrule,A,b,C,In,Out,Depth),
					(child(A,In,Out1,Depth),(child(b,Out1,Out2,Depth),child(C,Out2,Out,Depth))) ],
	sdcg_unset_option(maxdepth).
	
test_create_implementation_rule_with_parsetree_and_depth :-
	sdcg_set_option(parsetree),
	sdcg_set_option(maxdepth,10),
	create_implementation_rule(testrule,[A,b,C],[child(A),child(B),child(C)],ImplRule),
	ImplRule =.. [	:-,
					sdcg_rule(testrule,A,b,C,In,Out,[testrule(A,b,C),[Child1Parsetree,Child2Parsetree,Child3Parsetree]],Depth),
					(child(A,In,Out1,Child1Parsetree,Depth),(child(b,Out1,Out2,Child2Parsetree,Depth),child(C,Out2,Out,Child3Parsetree,Depth))) ],
	sdcg_unset_option(parsetree),
	sdcg_unset_option(maxdepth).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Depth check transformation tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_depth_check_transform :-
	% First create a selector rule
	create_selector_rule(testrule,2,[],SelectorRule),
	add_selector_depth_check(SelectorRule,ModSelectorRule),
	ModSelectorRule =.. [ :-,
		testrule([A,B,C,D,E]),
		(incr_depth(E,F),(msw(testrule(2),G),sdcg_rule(G,A,B,C,D,F)))
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Test various
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_extra_quotes :-
	extra_quotes('my string','\'my string\'').
	
print_list([]).
print_list([Item|Rest]) :-
	atom_codes(Item,Str),
	write(Str),nl,
	print_list(Rest).
	
	

test_quote_all :-
	assert(test(a)),
	assert(test(b)),
	findall(X,quote_all(test,X),L),
	%print_list(L),
	L==['\'a\'','\'b\''],
	retract(test(a)),
	retract(test(b)).	
	
	
	
	
