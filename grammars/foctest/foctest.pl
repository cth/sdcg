%
% I have a feeling that FOC becomes exponential. It seems like it with bigger grammars.
% This is a simple test which is meant to expose this.

sdcg ==> s(_,_).

s(Feature1,Feature2) ==>
	feat(Feature1),
	feat(Feature2).
	
feat(@enum([a,b,c,d,e,f,g,h,i,j,k,l,m,n],V)) ==> [V].
feat(A-B) ==> feat(A),feat(B).
	

	