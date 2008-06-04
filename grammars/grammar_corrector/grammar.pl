% This is a very simple grammar the grammar corrector.

sdcg ==> s(Parse,Correction). % start here.

tree(s(NPTree,))

s(s(np(NPNumber),vp(VPNumber)), s(np(NPNumber),vp(VPNumber))) ==> np(NPNumber), vp(VPNumber), {
	NPNumber == VPNumber.
}.

s(s(NPTree,VPTree), s(np(NPNumber,X),vp(NPNumber))) ==>
	np(NPNumber,NPTree), vp(VPNumber,VPTree),
	{
		NPNumber \== VPNumber.
	}.

s(s(np(NPNumber),vp(VPNumber)), s(np(VPNumber),vp(VPNumber))) ==> np(NPNumber,NP_Tree), vp(VPNumber,VP_Tree), {
	NPNumber \== VPNumber.
}.

vp(Number,vp(Tree)) ==> verb(_,Number,Tree).
np(Number,np(Tree)) ==> det, noun(_,Number,Tree).

det ==> [the].

% Tiny little lexicon:
noun(boy,sg,noun(boy,sg)) ==> [boy].
noun(boy,pl,noun(boy,pl)) ==> [boys].
noun(dog,sg,noun(dog,sg)) ==> [dog].
noun(dog,pl,noun(dog,pl)) ==> [dogs].
noun(cat,sg,noun(cat,sg)) ==> [cat].
noun(cat,pl,noun(cat,pl)) ==> [cats].

% So far, only transitive verbs
verb(eat,sg,verb(eat,sg)) ==> [eats].
verb(eat,pl,verb(eat,pl)) ==> [eat].
verb(play,sg,verb(play,sg)) ==> [plays].
verb(play,pl,verb(play,pl)) ==> [play].
verb(sleep,sg,verb(sleep,sg)) ==> [sleeps].
verb(sleep,pl,verb(sleep,pl)) ==> [sleeps].
