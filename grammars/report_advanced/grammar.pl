% A more advanced example SDCG grammar with regular expressions, macro expansions and 
% conditioning demonstrated.

% Demonstration sentences:
% workers put sacks into bins
% workers put sacks into bins on monday
% workers sleeps in sacks
% the workers work in teams
% the team works on mondays
% the boss put workers into work

headword(nohead).
headword(Word) :- lex(_,Word,_).

conditioning_mode(np(+,-,-)).
conditioning_mode(vp(+,-,-)).
conditioning_mode(det(+,-,-)).
conditioning_mode(noun(+,-,-)).
conditioning_mode(verb(+,-,-)).
conditioning_mode(preposition(+,-)).

expandmode(headword(+)).
expandmode(lex(-,+,+)).
expandmode(lex(-,+)).

start ==> s.
s ==> s, *(conjunctions).
conjunctions ==> conjunction, s.
s ==> 
	np(nohead,NPHead,Number),
	vp(BPHead,_VPHead,Number).

np(ParentHead,Head,Number) | @headword(W) ==>
	det(ParentHead,DetHead,Number),
	noun(DetHead,Head,Number).

np(ParentHead,Head,Number) | @headword(W) ==> 
	noun(ParentHead,Head,Number).

np(ParentHead,Head,pl) | @headword(W) ==> 
	noun(ParentHead,Head,_),
	conjunction,
	noun(ParentHead,_,_).

vp(ParentHead,Head,Number) | @headword(W) ==>
	verb(ParentHead,Head,Number),
	np(Head,_,_).

vp(ParentHead,Head,Number) ==>
	verb(ParentHead,_,Number),
	conjunction,
	vp(ParentHead,Head,Number).

conjunction ==> conjunction(_).
conjunction(@lex(conjunction,Word)) ==> [Word].
det(ParentHead,@lex(determiner,Word,Number)) | @headword(W) ==> [Word].
noun(ParentHead,@lex(noun,Word,Number)) | @headword(W) ==> [Word].
verb(ParentHead,@lex(verb,Word,Number)) | @headword(W) ==> [Word].
preposition(ParentHead,@lex(preposition,Word)) | @headword(W) ==> [Word].