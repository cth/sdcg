% Simple experiment with grammar correction
% We should be able to this deterministicly and then define the stochastic model afterwards

s(s(T1,T2)) ==> np(T1,N), vp(T2,N).
np(np(T1,T2),N) ==> det(T1,N), noun(T2,N).
vp(vp(T1,T2),N) ==> verb(T1,N), np(T2,_).
vp(vp(T1,T2),N) ==> verb(T1,N), adj(T2,_).

verb(verb(is),sg) ==> [ is ].
verb(verb(are),pl) ==> [ are ].

% Handle repeated words
noun(T,N) ==> single_noun(T,N).
noun(T,N) ==> double_noun(T,N).
single_noun(noun(sentence),sg) ==> [ sentence ].
double_noun(T,N) ==> single_noun(T,N), single_noun(T,N).

det(det(these),pl) ==> [ these ].
det(det(this),sg) ==> [ this ].

adj(adj(faulty),sg) ==> [ faulty ].
adj(adj(faulty),pl) ==> [ faulty ].
%adj(adj(faulty),sg) ==> [ nicest ].