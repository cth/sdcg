% A simple attribute grammar. 
% Only serves as and illustration of SDCG formalism.

noun(time,sg).
noun(flies,pl).
verb(flies,sg).
verb(crawls,sg).
verb(fly,pl).

start ==> s.
s  ==> np(_).
s  ==> np(N),vp(N).
np(N) ==> n(_Word1,sg),n(_Word2,N).
np(N) ==> n(_Word,N).
vp(N) ==> v(_Word,N),np(N).
vp(N) ==> v(_Word,N).

n(@noun(Word,Number)) ==> [Word].
v(@verb(Word,Number)) ==> [Word].