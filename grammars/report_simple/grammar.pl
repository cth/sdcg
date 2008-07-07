% A simple attribute grammar. 
% Only serves as and illustration of SDCG formalism.

start ==> s.
s  ==> np(_).
s  ==> np(N),vp(N).
np(N) ==> n(sg),n(N).
np(N) ==> n(N).
vp(N) ==> v(N),np(N).
vp(N) ==> v(N).

n(sg) ==> [time].
n(pl) ==> [flies].
v(sg) ==> [flies].
v(sg) ==> [crawls].
v(pl) ==> [fly].