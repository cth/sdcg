:- op(950, fx, ?). 
:- op(950, fx, *). 
:- op(950, fx, +). 

'?'(X) :-
	write('?'), write(X),nl.
'*'(X) :-
	write('*'), write(X),nl.
'+'(X) :-
	write('+'), write(X),nl.

star(X) :-
	write('star '), write(X),nl.

op_expand(C,+,R) :-
	C =.. [+,R].
op_expand(C,*,R) :-
	C =.. [*,R].
op_expand(C,?,R) :-
	C =.. [?,R].