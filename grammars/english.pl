% A relatively wide english grammar based on Jurafsky.
% NP Features: number, gender


sdcg ==> s.

s ==> np(Num1), vp(Num1).					% I prefer a morning flight
s ==> vp(Num1).							% Show the lowest fare
s ==> aux, np(Num), vp(Num).			% Do any of these flights have stops?
% wh-subject-question
s ==> wh(Num), np(Num), vp(Num).		% Which flights serve breakfeast?
% wh-non-subject-question
s ==> wh(Num1), np(Num1), aux(Num2), np(Num2), vp(Num2).	% What flights do you have from A to B.
% A very simple account of fronting:
s ==> pp, np(Num1).  				% On the table.
s ==> pp, s.						% On tuesday, it will happen.
% Conjuction of sentences:
s ==> s, conjunction, s.

vp(Num) ==> verb(Num), np.			% Prefer a morning flight
vp(Num) ==> verb, np, pp.		% Leave boston in the morning
vp ==> verb, pp.			% leaving on thursday
vp ==> vp, conjunction, vp. % What flights are leaving Denver and arriving in San Francisco.

pp ==> preposition, pp.		% from Los Angeles

% np ==> (det) (card) (ord) (quant) (AP) Nominal
np(Number) ==> nominal(Number).					% Mass nouns doesn't require a determiner (snow,dinner,water etc.)
np(Number) ==> det(Number), nominal(Number).
np(Number) ==> det(Number), qp(Number), nominal(Number).	% the two books
np(Number) ==> pronoun(Number).
np(Number) ==> np(Number), conjunction, np(Number).
np(Number) ==> ap(Number), nominal(Number).
np(Number) ==> det(Number), ap, nominal(Number).

% qp: Combinations of quantifiers, cardinals and nomimals
qp(Number) ==> ordinal(Number).
qp(Number) ==> cardinal(Number).
qp(Number) ==> quantifier(Number).
qp(Number) ==> qp(Number), qp(Number). % Combinations. We allow them all for now.

% Adjective phrases
ap ==> adjective.
ap ==> adjective, ap.

nominal ==> noun.
nominal ==> noun, nominal.
nominal ==> nominal, pp.
nominal ==> nominal, pp, pp.
nominal ==> nominal, pp, pp, pp.
% ... and so on
nominal ==> nominal, gerund_vp.
nominal ==> nominal, relative_clause.

gerund_vp ==> gerund_verb, np.
gerund_vp ==> gerund_verb, pp.
gerund_vp ==> gerund_verb.
gerund_vp ==> gerund_verb, np, pp.

relative_clause ==> relative_pronoun, vp.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simplified lexicon 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

noun ==> [flight].
noun ==> [flights].
noun ==> [fare].
noun ==> [dollar].
noun ==> [reservation].
noun ==> [breeze].

verb ==> [is].
verb ==> [are].
verb ==> [prefer].
verb ==> [like].
verb ==> [need].
verb ==> [want].
verb ==> [fly].
verb ==> [show].

wh ==> [which].
wh ==> [what].
wh ==> [who].

adjective ==> [expensive].
adjective ==> [cheap].
adjective ==> [cheapest].
adjective ==> [nonstop].
adjective ==> [first].
adjective ==> [latest].
adjective ==> [other].
adjective ==> [direct].

pronoun(sg,first,Gender) ==> [me].
pronoun(Number,Person,Gender) ==> [i].
pronoun(Number,Person,Gender) ==> [you].
pronoun(Number,Person,Gender) ==> [it].
pronoun(Number,Person,Gender)

relative_pronoun(Number,Person,Gender) ==> [that].
relative_pronoun(Number,Person,Gender) ==> [who].

proper_noun ==> [alaska].
proper_noun ==> [baltimore].
proper_noun ==> [los,angeles].
proper_noun ==> [chicago].
proper_noun ==> [new,york].

det ==> [a].
det ==> [the].
det ==> [an].
det ==> [this].
det ==> [these].
det ==> [that].

preposition ==> [from].
preposition ==> [to].
preposition ==> [in].
preposition ==> [on].
preposition ==> [at].
preposition ==> [near].

% auxilary verbs
aux ==> [ do ].
aux ==> [ does ].
aux ==> [ can ].
aux ==> [ will ].

gerund_verb ==> [ being ]. 
gerund_verb ==> [ preferring ].
gerund_verb ==> [ requiring ].
gerund_verb ==> [ leaving ].

cardinal ==> [one].
cardinal ==> [two].
cardinal ==> [three].
cardinal ==> [four].
cardinal ==> [five].
cardinal ==> [six].
cardinal ==> [seven].
cardinal ==> [eight].
cardinal ==> [nine].
cardinal ==> [ten].

ordinal ==> [first].
ordinal ==> [next].
ordinal ==> [second].
ordinal ==> [last].
ordinal ==> [other].
ordinal ==> [many]. % May only occur with plural count nouns

quantifier ==> [first,class].
quantifier ==> [nonstop].
quantifier ==> [longest].
quantifier ==> [earliest].

conjunction ==> [and].
conjunction ==> [or].
conjunction ==> [but].