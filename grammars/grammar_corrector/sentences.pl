sentence([the,boys,eat]).
sentence([the,boys,eat]).
sentence([the,cats,eat]).

correction([the,cat,eat],[the,cat,eats]).
correction([the,dogs,eats],[the,dogs,eat]).
correction([the,boy,eats],[the,dogs,eat]).

correct(Sentence,Correction) :-
	viterbi(s(Parse, _, Sentence, []),P1),
	viterbi(s(Correction,Parse,Sentence,[])).
	
%transform(Sentence,ParseTree) :
/*
(s 
	(np
		(det(the),
		(noun(dog,pl)))),
	(vp
		(verb(eat,sg)))).
		
		
(s 
	(np
		(det(the),
		(noun(dog,pl)))),
	(vp
		(verb(eat,pl)))).
*/		
