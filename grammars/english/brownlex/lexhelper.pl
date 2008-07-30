%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some helper rules for lexicon generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

tense(present).
tense(past).
tense(past-participle).

case(subjective).
case(objective).
case(possesive-determiner).
case(possesive-pronoun).
case(reflexive).

countable(countable).
countable(mass).

possesive(possesive-determiner).
possesive(possesive-nomimal).

subj_obj(subjective).
subj_obj(objective).

% Our own member relation, since using builtin causes FOC fail.
mem(X,[X|_]).
mem(X,[_|Y]) :- mem(X,Y).

words_for_tag(Tag,Word) :-
	tagword(Tag,Words),
	mem(Word,Words).
	
matchword(Tag,[Word],Word) :-
	match_inline(yes),
	words_for_tag(Tag,Word).
	
matchword(Tag,RuleBody,Word) :-
	not match_inline(yes),
	Body1 =.. [ {}, words_for_tag(Tag,Word) ],
	RuleBody = (Body1,[Word]).