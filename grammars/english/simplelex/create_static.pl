%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tool for converting the dynamic lexicon to a static lexicon,
% consisting only of ground atoms.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- cl('../../../sdcg.pl').

% Load the dynamic lexicon
:- [helpers,lexicon_dynamic].

:- convert_dynamic.

lex_rule_max_arity(10).
output_file('lexicon_static.pl').

lex_rules(0).
lex_rules(N) :-
	integer(N),
	unifiable_list(N,L),
	Call =.. [ lex | L ],
	catch(findall(L,Call,Instantiations),error(existence_error(procedure,lex/N),call/1),Instantiations=[]),
	write_static_lex_rules(Instantiations),
	N1 is N - 1,
	lex_rules(N1).

write_static_lex_rules([]).
write_static_lex_rules([Inst|Rest]) :-
	Rule =.. [ lex | Inst ],
	portray_clause(Rule),
	write_static_lex_rules(Rest).

convert_dynamic :-
	lex_rule_max_arity(A),
	output_file(File),
	open(File, write, Stream),
	current_output(PreviousStream),
	set_output(Stream),
	lex_rules(A),
	set_output(PreviousStream).