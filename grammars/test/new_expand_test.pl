v(a).
v(b).
v(c).

:- assert((a(X) :- b(X))).

conditioning_mode(s(+)).

start ==> s(a).
s(X) | @v(X) ==> s(X).
s(X) | @v(X) ==> [X].