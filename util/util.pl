%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Various general purpose utilities
% Author: Christian Theil Have <cth@itu.dk>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dot :- write('.').

write_n(0,_).
write_n(N,Atom) :-
	integer(N),
	M is N - 1,
	write(Atom),
	write_n(M,Atom).

% Tell me if X is a list (note, doesn't work with empty lists )
is_list(X) :-
	nonvar(X),
	functor(X,'.',_).
	
list_item(X,[X]).

% Test if X is inclosed in curly brackets
is_code_block(X) :-
	nonvar(X),
	functor(X, {}, _).

% Test if clause has multiple constituents
is_composed(X) :-
	functor(X, ',', _).
	
atom_append(Atom1,Atom2,Atom3) :-
	atom_codes(Atom1,A1Codes),
	atom_codes(Atom2,A2Codes),
	append(A1Codes,A2Codes,A3Codes),
	atom_codes(Atom3,A3Codes).
	
% Create a list of unground vars, which can subsequently be used
% to unify with something.
unifiable_list(0,[]).
unifiable_list(Len,[_A|L]) :-
	NewLen is Len - 1,
	unifiable_list(NewLen,L), !.

% Converts a clause to a list of rules
clause_to_list((E1,E2), [E1|Rest]) :- clause_to_list(E2, Rest).
clause_to_list(E, [E]).

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
	
double_append(Lst1,Lst2,Lst3,Lst4) :-
	append(Lst1,Lst2,TmpLst),
	append(TmpLst,Lst3,Lst4).

% assert_once always succeds
assert_once(C) :-
	(not(clause(C,_)) -> assert(C) ; true).

atomlist_to_clauses([],_,[]).
atomlist_to_clauses([Atom],Arity,[Clause]) :-
	unifiable_list(Arity,L),
	Clause =.. [ Atom | L].

% Retract each clause in a list
retract_each([]).
retract_each([C|R]) :-
	write('Trying to retract '), write(C),nl,
	retractall(C),
	retract_each(R).

replacement_name(Name, Arity, Number, Conditions, NewName) :-
	hyphenate(Name, Arity, N1),
	hyphenate(N1,Number,N2),
	NewName =.. [ N2 | Conditions ].

replacement_name(Name, Arity, Number, NewName) :-
	hyphenate_list([Name, Arity, Number], NewName).
	
hyphenate_list([A],A).
hyphenate_list([A,B|R],H) :-
	hyphenate(A,B,C),
	hyphenate_list([C|R],H).

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
	
remove_ground([],[]).
remove_ground([V|R],NotGround) :-
	remove_ground(R,NotGroundRest),
	(ground(V) ->
		NotGround = NotGroundRest
	;
		append([V],NotGroundRest,NotGround)
	).

% Removes empty list elements from a list
remove_empty([],[]).
remove_empty([[]|R],NonEmpty) :- remove_empty(R,NonEmpty).
remove_empty([E|R],[E|NonEmpty]) :- remove_empty(R,NonEmpty).

% Flatten a list at depth one. 
flatten_once([],[]).
flatten_once([[]|R],FlatRest) :- flatten_once(R,FlatRest).
flatten_once([[E|ER]|R],[E|FlatRest]) :-
	flatten_once(R,FlatR),
	append(ER,FlatR,FlatRest).

% Converts a list of rules to a clause
list_to_clause([E],(E)).
list_to_clause([E|L],(E,ClauseRest)) :-
	list_to_clause(L,ClauseRest).


/* defunct
clause_to_list(A,[A]).
clause_to_list((A,B),L) :-
	(functor(B,',',2) ->
		clause_to_list(B,C),
		L = [A|C]
	;
		L = [A,B]
	).
*/

% Outputs each element of a list on a separate line
write_list([]).
write_list([E|Rest]) :-
	write(E),nl,
	write_list(Rest).
	
% Given a list of argument, add_functor will
% create a term from the functor and argument list
add_functor(_,[],[]).
add_functor(F,[E|R],[Functorized|FunctorizedRest]) :-
	Functorized =.. [ F | E ],
	add_functor(F,R,FunctorizedRest).

combine([L], L).
combine([L1,L2],Combined) :-
        combine_two(L1,L2,Combined).
combine([L1,L2|R], Combined) :-
        combine(R,CR),
        combine_two(L1,L2,C1),
		combine_two(C1,CR,Combined), !.

combine_two([],_,[]).
combine_two(_,[],[]).
combine_two([E1],[E2], Combined) :-
	append(E1,E2,C),
	Combined = [ C ].
combine_two([E1|R1], [E2|R2], Combined) :-
	combine_two([E1],[E2],C),
	combine_two(R1,[E2|R2], CombLeft),
	combine_two([E1|R1],R2, CombRight),
	union(C,CombLeft,C1),
	union(C1,CombRight,Combined).

% True is the two sets are equal
set_equal(Set1,Set2) :-
	subtract(Set1,Set2,[]).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Various utilities which are not directly used by the compiler,
% but is practical when creating grammars.
% Most of these functions can be applied with @ expansions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Allows creating rules like
% r(@enum([a,b],AB) ==> [AB].
% which expands to: r(a) ==> [a]. and r(b) ==> [b].
enum(List,Value) :-
	ground(List), 
	member(Value,List), !.

enum(Value,Value).
	
enum_q(List,QuotedValue) :-
	enum(List,Value),
	extra_quotes(Value,QuotedValue).

exclude(Pred,Atom,Value) :-
	atom(Atom),
	exclude(Pred,[Atom],Value).
exclude(Pred,List,Value) :-
	C =.. [ Pred, Value],
	call(C),
	not member(Value,List).

quote_all(Pred,Quoted) :-
	ToCall =.. [Pred,AtomValue],
	call(ToCall),
	extra_quotes(AtomValue,Quoted).
	
quote_all_twice(Pred,QuotedTwice) :-
	quote_all(Pred,QuotedOnce),
	extra_quotes(QuotedOnce,QuotedTwice).

%q_string_all(Pred,QString) :-
%	stringify(Pred,String),
%	extra_quotes(String,QString).

extra_quotes(InputAtom,Quoted) :-
	atom_codes(InputAtom,Str),
	atom_codes('\'',Quote),
	append(Quote, Str, QuoteStr),
	append(QuoteStr,Quote,QuotedString),
	atom_codes(Quoted,QuotedString).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Loading files and resolving paths
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
resolve_path(Relative,Absolute) :-
	sdcg_directory(Basedir),
	atom_codes(Basedir,BasedirChars),
	atom_codes(Relative,RelativeChars),
	append(BasedirChars,RelativeChars,AbsoluteChars),
	atom_codes(Absolute,AbsoluteChars).

% require compiles and loads a file within the distribution based
% on a path relative to the base directory
require(File) :-
	resolve_path(File,AFile),
	cl(AFile).

include_rel(File):-
	resolve_path(File,AFile),
	[AFile].