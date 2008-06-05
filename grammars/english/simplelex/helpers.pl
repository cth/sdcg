%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some helper rules for lexicon generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

num(plur).
num(sing).

gender(masc).
gender(fem).
gender(neut).

human(male).
human(female).

person(first).
person(second).
person(third).

case(subjective).
case(objective).
case(possesive-determiner).
case(possesive-pronoun).
case(reflexive).

possessive(possesive-determiner).
possessive(possesive-nomimal).
subj_obj(subjective).
subj_obj(objective).

% expand_mode(enum(+,-)).
enum(List,Value) :-
	ground(List), 
	member(Value,List).

exclude(Pred,Atom,Value) :-
	atom(Atom),
	exclude(Pred,[Atom],Value).
exclude(Pred,List,Value) :-
	C =.. [ Pred, Value],
	call(C),
	not member(Value,List).

stringify(Pred,StringValue) :-
	ToCall =.. [Pred,AtomValue],
	call(ToCall),
	atom_chars(AtomValue,StringValue).