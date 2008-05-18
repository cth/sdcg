% generate_consumes(A,B,[en,lang,liste],R).
% R = consume(A,en,_dac),consume(_dac,lang,_e84),consume(_e84,liste,B) ?
% A = [ en,lang,liste | B ],

m(A,B,L) :-
	append(L,B,A).
