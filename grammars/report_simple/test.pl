rule(np, A, B) :-
	n(sg, C),
	n(A, D),
	ap(C,D,B).

n(sg,[fly]).
n(pl,[flies]).

ap([], A, A).
ap([A|B], C, [A|D]) :-
        ap(B, C, D).

