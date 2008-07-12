% A more advanced example SDCG grammar with regular expressions, macro expansions and 
% conditioning demonstrated.

% Demonstration sentences:
% workers put sacks into bins
% workers put sacks into bins on monday
% workers sleep in sacks
% the workers work in teams
% the team works on mondays
% the boss put workers into work

headword(nohead).
headword(Word) :- lex(_,Word,_).
headword(Word) :- lex(_,Word).

conditioning_mode(np(+,-,-)).
conditioning_mode(vp(+,-,-)).
conditioning_mode(pp(+,-,-)).
conditioning_mode(det(+,-,-)).
conditioning_mode(noun(+,-,-)).
conditioning_mode(verb(+,-,-)).
conditioning_mode(preposition(+,-)).

expandmode(headword(+)).
expandmode(lex(-,+,+)).
expandmode(lex(-,+)).

start ==> story.
story ==> +(sentence).
sentence ==> sentence, conjunction(nohead,_), sentence.
sentence ==> 
	np(nohead,NPHead,Number),
	vp(NPHead,_VPHead,Number).

% Noun phrases with determiners
np(ParentHead,Head,Number) | @headword(W) ==>
	det(ParentHead,DetHead,Number),
	noun(DetHead,Head,Number).

% singular nouns
np(ParentHead,Head,Number) | @headword(W) ==> 
	noun(ParentHead,Head,Number).

% Conjunctions of noun phrases
np(ParentHead,Head,pl) | @headword(W) ==> 
	noun(ParentHead,Head,_),
	conjunction(ParentHead,_),
	noun(ParentHead,_,_).

% Noun phrases with prepositions
np(ParentHead,Head,Number) | @headword(W) ==>
	noun(ParentHead,Head,Number),
	pp(Head,_PPHead).
	
% Intransitive verbs
vp(ParentHead,Head,Number) | @headword(W) ==>
	verb(ParentHead,Head,Number).

% Transitive verbs
vp(ParentHead,Head,Number) | @headword(W) ==>
	verb(ParentHead,Head,Number),
	np(Head,_,_).

% Conjunctions in verb phrases
vp(ParentHead,Head,Number) | @headword(W) ==>
	verb(ParentHead,_,Number),
	conjunction(ParentHead,_),
	vp(ParentHead,Head,Number).
	
pp(ParentHead,Head,Number) | @headword(W) ==>
	preposition(ParentHead,Head),
	np(Head,_NPHead,Number).
	
conjunction(ParentHead,@lex(conjunction,Word)) ==> [Word].
det(ParentHead,@lex(determiner,Word,Number)) | @headword(W) ==> [Word].
noun(ParentHead,@lex(noun,Word,Number)) | @headword(W) ==> [Word].
verb(ParentHead,@lex(verb,Word,Number)) | @headword(W) ==> [Word].
preposition(ParentHead,@lex(preposition,Word)) | @headword(W) ==> [Word].