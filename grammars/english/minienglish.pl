% A relatively wide coverage english grammar based on mainly Jurafsky chap 9.
% Note to self: Check if @ expansion can expand to a list of Unification symbols

sdcg ==> s(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% s: Sentence types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
s(np(NPTree,Number,Person,Gender),vp(VPTree,Number,Tense,Person)) ==>
	np(NPTree,Number,Person,Gender),
	vp(VPTree,Number,Tense,Person).
	

s(vp(Tree,Number,Tense,Person)) ==>
	vp(Tree,Number,Tense,Person).
/*	

s((verb(VNumber,VTense,VPerson,to_do),np(NPTree,Number,Person,Gender),vp(VPTree,Number,present,Person))) ==>
	verb(VNumber,VTense,VPerson,to_do),
	np(NPTree,Number,Person,Gender),
	vp(VPTree,Number,present,Person).

% have: VP in past tense --	Have any people come?, have you done it?
% had: VP in past tense -- Had they thought about it?
s((verb(VNumber,VTense,VPerson,to_have),np(NPTree,Number,Person,Gender),vp(VPTree,Number,past,Person))) ==>
	verb(VNumber,VTense,VPerson,to_have),
	np(NPTree,Number,Person,Gender),
	vp(VPTree,Number,past,Person).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% vp: Verb phrases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%vp((verb(Number,Tense,Person,Stem)),Number,Tense,Person) ==>
%	verb(Number,Tense,Person,Stem).
vp((verb(VNumber,VTense,VPerson,VStem),np(NPTree,NPPerson,NPNumber,objective)), VNumber,VTense,VPerson) ==>
	verb(VNumber,VTense,VPerson,VStem),
	np(NPTree,NPPerson,NPNumber,objective).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% np: Noun phrases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
np(nominal(NomTree,Person,Number,Case),third,Number,Case) ==>
	nominal(NomTree,Person,Number,Case).

% np --> det, nominal
np((det(det(Number,Gender,Stem1)),nominal(NomTree,Person,Number,Gender)),Number,Person,Gender) ==>
	det(Number,Gender,Stem1),
	nominal(NomTree,Person,Number,Gender).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Nominals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nominal(noun(Countable,Number,Gender,Stem),Countable,Number,Gender) ==>
	noun(Countable,Number,Gender,Stem).
nominal((noun(Countable1,Number1,Gender1,Stem1),nominal(NomTree,Countable2,Number2,Gender2)), Countable2,Number2,Gender2) ==>
	noun(Countable1,Number1,Gender1,Stem1),
	nominal(NomTree,Countable2,Number2,Gender2).
