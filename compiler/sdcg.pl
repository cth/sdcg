%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Stochastich DCG Compiler
% Author: Christian Theil Have <cth@itu.dk>
% 
% Compiles a DCG to a PRISM program which works as a stochastic
% version of the original DCG.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op(1200, xfx, ==>).

:- dynamic sdcg_user_option/2.
:- dynamic sdcg_start_definition/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SDCG Options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sdcg_default_option(start_symbol, sdcg).
sdcg_default_option(prism_file, 'generated_sdcg.psm').
sdcg_default_option(debug,no).

sdcg_option(Opt, Val) :-
	(clause(sdcg_user_option(Opt,_),_) -> sdcg_user_option(Opt,Val) ; sdcg_default_option(Opt,Val)).

sdcg_set_option(Opt,Val) :-
	retractall(sdcg_user_option(Opt,_)), % Discard old value if present
	assert(sdcg_user_option(Opt,Val)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Clause translation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-->(LHS,RHS) :- ==>(LHS,RHS).

==>(LHS,RHS) :-
	sdcg_debug((write('processing rule '), write(LHS), write(' ==> '), write(RHS), nl)),
	compose_list(RHS, RHS_L1),
	compact_list(RHS_L1,RHS_L),
	sdcg_option(start_symbol,StartSymbol),
	(functor(LHS,StartSymbol,_) ->
		sdcg_debug((write('this looks like a start rule'), nl)),
		rewrite_start_rule(LHS,RHS_L)
	;
		rewrite_rule(LHS,RHS_L)
	).

rewrite_start_rule(LHS,RHS) :-
	% The symbol "StartSymbol" is the root of the tree and must be unique, so throw
	% an exception if this is not the case.
	sdcg_option(start_symbol,StartSymbol),
	(clause(values(sdcg_start_definition,Def),_) -> throw(error(sdcg_start_symbol_already_defined(Def))) ; true),
	% Rewrite with difference list:
	LHS =.. [ StartSymbol | Features ],
	append(Features,[InList,OutList], FeatWithDiffList),
	NewLHS =.. [ StartSymbol | FeatWithDiffList ],
	rewrite_rule_rhs(InList,OutList,RHS,NewRHS),
	StartRule =.. [ :-,  NewLHS, NewRHS ],
	% Assert rule and definition rule:
	assert(StartRule),
	functor(NewLHS,_,Arity),
	assert(sdcg_start_definition(StartSymbol/Arity)).
	
rewrite_rule(LHS,RHS) :-
	% Expand values(Rulename, ...) to include this new nonterminal
	% and give it a unique new name
	functor(LHS,Name,Arity),
	LHS =.. [ Name | Features],
	expand_values(Name, Arity, NewName),

	% Generate the selection rule, which stochastically selects
	% which implementation rule to use.
	MSW =.. [ Name, Arity ],
	values(MSW, Values),
	RHS1 =.. [ msw, MSW, SwitchVar ],
	unifiable_list(Arity,EmptyFeatures),
	append(EmptyFeatures,[InList,OutList], EmptyFeaturesWithDiffList),
	generate_selector(EmptyFeaturesWithDiffList,SwitchVar,Values,RHS2),
	NewRHS = (RHS1,RHS2),
	append([Name],EmptyFeaturesWithDiffList,NewLHSList),
	NewLHS =.. NewLHSList,
	SelectorRule =.. [ :-, NewLHS, NewRHS ],
	sdcg_debug((write('Selector Rule: '), write(SelectorRule), nl)),
	% Remove old selector rule if there is one
	(clause(NewLHS,OldBody) -> retract((NewLHS :- OldBody)); true),
	assert(SelectorRule), % Add new Selector to database

	% Generate the implementation rule with the name "NewName"
	append(Features,[InList,OutList],NewFeatures),	
	ImplRuleLHS =.. [ NewName | NewFeatures ],
	rewrite_rule_rhs(InList,OutList,RHS,ImplRuleRHS),
	ImplRule =.. [ :-, ImplRuleLHS, ImplRuleRHS ],
	sdcg_debug((write('Implementation Rule: '), write(ImplRule),nl)),
	assert(ImplRule).
	
% Selector is used to build the RHS of those nonterminal 
% rules which select the actual nonterminal to be used
generate_selector(Features, SwitchVariable, [SwitchValue], Selector) :-
	Condition =.. [ ==, SwitchVariable, SwitchValue ],
	append([SwitchValue],Features,ConsList),
	Consequence =.. ConsList,
	Selector =.. [ ->, Condition, Consequence ].
	
generate_selector(Features, SwitchVariable, [SwitchValue|R], Selector) :-
	generate_selector(Features,SwitchVariable,[SwitchValue],S1),
	generate_selector(Features,SwitchVariable,R,S2),
	Selector =.. [ ;, (S1), (S2) ].
	
rewrite_rule_rhs(InOut,InOut,[R],Body) :-
%	write('code block encountered'), write(R),nl,
	R =.. [{},Body].

% FIXME Tomorrow.
%rewrite_rule(In,Out,[R],Body) :-
%	R =.. [->,Clau]

rewrite_rule_rhs(In, Out, [R], Body) :-
	(is_list(R) ->
		generate_consumes(In,Out,R,Body) % Note, this forces lexicalization!

	;is_composed(R) ->
		write('composed rules encountered'), write(R),nl
		
	;is_code_block(R) ->
		write('code block encountered'), write(R),nl,
		R =.. [{},Body], In = Out

	;% Otherwise normal
		R =.. L,
		append(L,[In,Out],L1),
		Body =.. L1
	).

rewrite_rule_rhs(In, Out, [R | RHS], Body) :-
	rewrite_rule_rhs(In,NextIn,[R],Clause),!,
	rewrite_rule_rhs(NextIn,Out, RHS,ClausesRest),
	Body = (Clause, (ClausesRest)).
	
	
generate_consumes(In,Out,[T],Clause) :-
	Clause =.. [ consume, In, T, Out ].
generate_consumes(In,Out,[T|R],Clauses) :-
	generate_consumes(In,NextIn,[T],C1),!,
	generate_consumes(NextIn,Out,R,CR),
	Clauses = (C1, (CR)).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Loading and compiling SDCG from file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sdcg(File) :-
	sdcg_parse(File), !,
	sdcg_debug(listing),
	sdcg_compile.
	
sdcg_parse(File) :-
	sdcg_debug((write('Loading SDCG in '), write(File), nl)),
	open(File, read, Stream),
	ground(Stream),
	read_rules(Stream, Rules),
	compile_rules(Rules),
	close(Stream).

read_rules(Stream, Rules) :-
	read(Stream, T),
	((T == end_of_file) ->
		Rules = []
	;
		read_rules(Stream,Rest),
		append([T],Rest,Rules)
	).

compile_rules([]).
compile_rules([X|R]) :-
	functor(X,F,A),
	(member([F,A], [[==>,2],[sdcg_set_option,2]]) ->
		call(X)
	;
		assert(X)
	),
	compile_rules(R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prism program generation:
% Generate a prism program from the asserted clauses
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sdcg_compile :-
	sdcg_debug((write('compiling SDCG to PRISM program'),nl)),
	sdcg_option(prism_file, OutFile),
	write_prism_program_to_file(OutFile),
	prism(OutFile).

write_prism_program :-
	write_prism_program(user_output).
	
% Doesn't work:
simulate_write_prism_program :- 
	open_null_stream(S),
	write_prism_program(S).

write_prism_program_to_file(FileName) :-
	open(FileName,write,Stream),
	write_prism_program(Stream),
	close(Stream).

write_prism_program(Stream) :-
	current_output(PreviousStream),
	set_output(Stream),
	write_prism_directives,
	section('MSW declarations'), 
	listing(values/2),
	write_sdcg_start_rule,
	section('Non-terminals'),
	catch(findall(MSW,values(MSW,_), Selectors),_,true),
	write_rules(Selectors),
	retractall(values(_,_)), % Remove all asserted values clauses
%	write_consume,
	section('User defined'),
	listing,
	set_output(PreviousStream).

write_prism_directives :-
	% Make sure there is we have sdcg_start symbol
	(not(clause(sdcg_start_definition(_),_)) -> throw(error(no_sdcg_start_symbol)) ; true),
	sdcg_start_definition(Name/Arity),
	ground(Name),
	ground(Arity),
	section('PRISM directives'),
	writeq(target(failure,0)),dot,nl,
	write((failure :- not success(_))),dot,nl,
	ExceptOutArity is Arity - 1,
	unifiable_list(ExceptOutArity, L1),
	append(L1,[[]],L2),
	StartCall =.. [ Name | L2 ],
	write(success(StartCall)),dot,nl,
	writeq(data(user)),dot, nl.

write_sdcg_start_rule :-
	section('Start symbol'),
	sdcg_start_definition(StartSymbol/Arity),
	listing(StartSymbol/Arity),
	unifiable_list(Arity,L),
	Head =.. [ StartSymbol | L],
	retractall(Head),
	retractall(sdcg_start_definition(_)).

write_rules([]).
write_rules([N|R]) :-
	% Write the rule for nonterminal N:
	functor(N,Name,1),
	N =.. [ Name, Arity ],
	NewArity is Arity + 2, % account for the added in and out lists
	listing(Name/NewArity),
	% Write all derived rules if this is a selector rule (official non-terminal):
	values(N, Derived),
	write_implementation_rules(NewArity,Derived),
	% Write the rest of the nonterminals:
	write_rules(R),
	% Remove this rule:
	remove_rule(Name,NewArity).

write_implementation_rules(_,[]).
write_implementation_rules(Arity,[N|R]) :-
	listing(N/Arity),
	write_implementation_rules(Arity,R),
	% Remove the implementation from database:
	remove_rule(N,Arity).

remove_rule(N,Arity) :-
	unifiable_list(Arity,L),
	CL =.. [ N | L],
	(clause(CL,Body) -> retract((CL :- Body));
	throw(error(remove_rule(N/Arity))), write(CL), nl).

section(S) :-
	nl,write('%%%  '),write(S),write('  %%%'),nl.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dot :- write('.').

is_list(X) :- % doesn't work with empty lists %	
	nonvar(X),
	functor(X,'.',_).

is_code_block(X) :-
	nonvar(X),
	functor(X, {}, _).

is_composed(X) :-
	functor(X, ',', _).

unifiable_list(0,[]).
unifiable_list(Len,[_A|L]) :-
	NewLen is Len - 1,
	unifiable_list(NewLen,L), !.

compose_list((E1,E2), [E1|Rest]) :- compose_list(E2, Rest).
compose_list(E, [E]).

compact_list([],[]).
compact_list([L],[L]).
compact_list([L1,L2|Rest], CL) :-
	is_list(L1), 
	is_list(L2),
	append(L1,L2,L3),
	compact_list([L3|Rest],CL).
compact_list([L1,L2|Rest],CL) :-
	compact_list([L2|Rest],CL1),
	append([L1],CL1,CL).


% The argument clause is only called if sdcg_option(debug,yes).
% Any write's from sdcg_debug goes to STDOUT, no matter current output stream
sdcg_debug(Clause) :-
	current_output(PreviousStream),
	set_output(user_output),
	(sdcg_option(debug,yes) -> call(Clause)
	; true),
	set_output(PreviousStream).

% assert_once always succeds
assert_once(C) :-
	(not(clause(C,_)) -> assert(C) ; true).
	replacement_name(Name, Arity, Number, NewName) :-
	hyphenate(Name, Arity, N1),
	hyphenate(N1,Number,NewName).

% Stitch together the first tree arguments
% as the fourth: eg. 
% hyphenate(noun, 3, 2, noun_3_2) => true
hyphenate(First, Second, Hyphenated) :-
	ground(First),
	ground(Second),
	Hyphen = "_",
	name(First, FirstList),
	name(Second, SecondList),
	append(FirstList, Hyphen, L1),
	append(L1, SecondList, L2),
	name(Hyphenated, L2).

% Hyphenate also works in the opposite direction
% Eg. hyphenate(O,A,N,noun_3_2)	 => O=noun, A=3, N=2
hyphenate(First,Second,Hyphenated) :-
	ground(Hyphenated),
	peel_rightmost(Hyphenated, FirstList, SecondList),
	name(First,FirstList),
	name(Second,SecondList).

% Peels of the rightmost hyphen and arg
% eg. peel("test_1_2", "test_1", "2") => true
peel_rightmost(Input, Rest, Arg) :-
	"_" = [ Hyphen ],
	(is_list(Input) -> InputCodes = Input ; atom_codes(Input, InputCodes)),
	reverse(InputCodes,Reversed),
	append(A,B,Reversed),
	B = [ Hyphen | _], 
	reverse(A,Arg),
	append(Rest,[ Hyphen | Arg],InputCodes), !.

expand_values(Name, Arity, NewName) :-
	MSW =.. [Name, Arity],
	% If the MSW is not defined yet we might get an error
	catch((values(MSW,Values);true), _,true),
		(ground(Values) -> 
		% then extract largest id from the values
		Values = [ First | _ ], % New elements are always inserted in the beginning of the list
		peel_rightmost(First, _, Peeled),
		number_codes(LastId,Peeled),
		NextId is LastId + 1
		% Else: Just start with a new id
			; NextId = 1),
	replacement_name(Name, Arity, NextId, NewName),
	update_msw(MSW, NewName).


update_msw(Name, Value) :-
	% Extract the values of L but if values is not defined yet (exception thrown)
	% then just assert it. 
	catch(	(values(Name,L) -> true; asssert(values(Name,[Value]))), _, assert(values(Name,[Value]))),
	(ground(L) -> true ; L = [Value]),		% Make sure L is grounded
	(member(Value,L) -> true ;
		retract(values(Name,_)),
		NewValues = [Value|L],
		assert(values(Name,NewValues))).
	% consume/3 is referenced by the generated program

consume([Token|R],Token, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Test stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
try_it :-
	(sdcg(X) ==> np(X)),
	(np(X) ==> det(X), noun(X)),
	(np(X) ==> noun(X)),
	(noun(sg) ==> [dog]),
	(noun(pl) ==> [dogs]),
	(det(pl) ==> [the]),
	(det(sg) ==> [the]),
	sdcg_compile.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some unit tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ideally we would build this automatically, but clause(X,_) doesn't work in BProlog :-(
test_clauses([	compact_list, compose_list, unifiable_list, peel_rightmost,
				update_msw, hyphenate, replacement_name ]).

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Known bugs
% 
% 6. Doesn't support (a -> b, c) syntax yet.
%
% 7. Write detailed set of test cases.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Other stuff:
/*
Consider again the difference between terminals and nonterminals:
To see the problem consider this grammar:
a ==> b, noun
a ==> b, [dog]
noun ==> [dog]

It's really the same difference! But we cannot know for sure, I think.
It's what i'm generating now actually a lexicalized model????
*/
