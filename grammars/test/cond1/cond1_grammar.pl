% A very simple conditioning example
v(a).
v(b).
v(c).

conditioning_mode(s(+)).

start ==> s(a).
%s(X) | @v(X) ==> s(X).
s(X) | @v(X) ==> [one].
s(X) | @v(X) ==> [two].