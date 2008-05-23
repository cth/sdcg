% A new grammar for use cases

sdcg ==> s.

s ==> np, vp.				% I prefer a morning flight
s ==> vp.					% Show the lowest fare
s ==> aux, np, vp.			% Do any of these flights have stops?

% wh-subject-question
s ==> wh, np, vp.			% Which flights serve breakfeast?

% wh-non-subject-question
s ==> wh, np, aux, np, vp.	% What flights do you have from A to B.

% FIXME: We need fronting as well.
s ==> pp, np.
s ==> pp, s.

vp ==> verb, np.			% Prefer a morning flight
vp ==> verb, np, pp.		% Leave boston in the morning
vp ==> verb, pp.			% leaving on thursday

pp ==> preposition, pp.		% from Los Angeles

% np ==> (det) (card) (ord) (quant) (AP) Nominal
np ==> det, nominal.		
np ==> det,cardinal,nominal.	% the two books
np ==> det,ordinal,nominal. 	% the next day 
np ==> det,quantifier,nominal.	% tbe longest flight
np ==> propernoun.

% Adjective phrases
ap ==> adjective.

nominal ==> noun.
nominal ==> noun, nominal.

det ==> [a].
det ==> [the].

noun ==> [ flight ].

wh ==> [which].
wh ==> [what].
wh ==> [who].

adjective ==> [ expensive ].
adjective ==> [ cheap ].

% auxilary verbs
aux ==> [ do ].
aux ==> [ does ].
aus ==> [ can ].
