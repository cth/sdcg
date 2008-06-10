% An attempt to create a simple lexicalized grammar in SDCG
% Based on chomsky example
%
% Maybe I have to do expansions for each kind of rule

s(Head) ==> np(_), vp(Head).

np(Head) ==> adjp(_), noun(Head).
np(Head) ==> det(_), noun(Head).

adjp(Head) ==> adj(Head).
adjp(Head) ==> adjp(Head),adjp(_).

vp(Head,Headword) ==> verb(Headword).
vp(Head,Headword) ==> verb(Head), adverb(_).

%% Lexicon

verb(sleep) ==> [ sleep ].
verb(rage) ==> [ rage ].
verb(talk) ==> [ talk ].
verb(write) ==> [ write ].
verb(work) ==> [ work ].

noun(programmers) ==> [ programmers ].
noun(ideas) ==> [ ideas ].
noun(fanatics) ==> [ fanatics ].  

adj(productive) ==> [ productive ].
adj(colorless) ==> [ colorless ].
adj(green) ==> [ green ].
adj(funny) ==> [ funny ]. 

adverb(furiously) ==> [ furiously ].
adverb(well) ==> [ well ].
adverb(constantly) ==> [ constantly ]. 
adverb(badly) ==> [ badly ].

det(the) ==> [ the ].
