% A relatively wide coverage english grammar based on mainly Jurafsky chap 9.
% Note to self: Check if @ expansion can expand to a list of Unification symbols

sdcg ==> s.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% s: Sentence types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Basic np vp sentences
% Example: I prefer the morning flight
s ==>
	np(_,Number,Gender,_),
	vp(_,Number,Gender,_).

% Imperative structure verb sentences. Have no subject.
% Example: Show the lowest fare
s ==> vp(Num1).

s ==> aux, np(Num), vp(Num).			% Do any of these flights have stops?
% wh-subject-question
s ==> wh(Num), np(Num), vp(Num).		% Which flights serve breakfeast?
% wh-non-subject-question
s ==> wh(Num1), np(Num1), aux(Num2), np(Num2), vp(Num2).	% What flights do you have from A to B.

% A very simple account of fronting:
s ==> pp, np(Num1).  				% On the table.
s ==> pp, comma, s.					% On tuesday, it will happen.
% Conjuction of sentences:
s ==> s, conjunction, s.
s ==> start_enclose, s, end_enclose.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% vp: Verb phrases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features: vp(Tree,Number,Tense,Person)

% vp --> verb, np.
% Number between the verb and np doesn't have to agree, but the vp takes it's number from the verb. This also goes for tense and person.
% Examples:
% Prefer a morning flight.
% Help yourself.
vp((verb(VNumber,Tense,VPerson,VStem),np(NPTree,NPPerson,NPNumber,@enum([objective,reflexive],Case), NPCase), VNumber,VTense,VPerson) ==>
	verb(VNumber,VTense,VPerson,VStem),
	np(NPTree,NPPerson,NPNumber,Case).
	
% vp --> verb, np, pp.
% Examples:
% Leave boston in the morning
% Read books on natural language in the evenings.
vp((verb(VNumber,VTense,VPerson,VStem),np(NPTree,NPPerson,NPNumber,NPCase),pp(PPTree,PPCountable,PPNumber,PPGender)), VNumber,VTense,VPerson) ==>
	verb(VNumber,VTense,VPerson,VStem),
	np(NPTree,NPPerson,NPNumber,NPCase),
	pp(PPTree,PPCountable,PPNumber,PPCase).

% vp --> verb,pp.
% work on thursday
vp((verb(VNumber,VTense,VPerspon,VStem),pp(PPTree,PPCountable,PPNumber,PPGender)),VNumber,VTense,VPerson) ==>
	verb(VNumber,VTense,VPerson,VStem),
	pp(PPTree,PPCountable,PPNumber,PPCase).

% vp --> vp,conjunction,vp.
% Examples:
% I [like pizza but hate olives].
% He [loved Mary and loves Elizabeth]: Tense does not have to agree
% * Love B and loves B: Person/Number does not agree
% We will try to relax these constraints later to see if i am wrong about this
% Questionable examples:
% (The flights) _are_ leaving Denver and _arriving_ in San Francisco. : This might be an other rule, e.g. vp --> to_be_verb, gerund conj gerund
% (The flights) are leaving Denver and arrive in San Francisco.
% *(The flights) leave Denver and arriving in San Francisco.
% Do we need some agreement here? Problem is that the above sentence uses gerund verbs.
vp((vp(Tree1,Number,Tense1,Person),conjunction(Stem),vp(Tree2,Number,Tense2,Person)),Number,Tense,Person) ==>
	vp(Tree1,Number,Tense1,Person),
	conjunction(Stem),
	vp(Tree2,Number,Tense2,Person).
	
vp ==>
	verb(Number1,Tense1,Person1,@),
	[is],
	gerund_vp.
	
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gerund verbs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gerund_vp((gerund_verb(Stem), np)) ==> 
	gerund_verb(Stem),
	np(Number,...).
	
gerund_vp ==>
	gerund_verb,
	pp.
gerund_vp ==>
	gerund_verb.
gerund_vp((gerund_verb(Stem),np(NPTree,Number,Person,Gender))) ==> 
	gerund_verb(Stem),
	np(NPTree,Number,Person,Gender), pp.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% np: Noun phrases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features:
% - ParseTree
% - Number: sing/plur
% - Person: 
% Questions to self: Should np have person? - yes, since they can contain pronouns
% Then, can we assume all common are third person? - They are in brown, so yes.

% np ==> (det) (card) (ord) (quant) (AP) Nominal

% np --> nominal
% Mass nouns doesn't require a determiner (snow,dinner,water etc.)
np(nominal(NomTree,Person,Number,Case),third,Number,Case) ==>
	nominal(NomTree,Person,Number,Case).

% np --> det, nominal
np((det(det(Number,Gender,Stem1)),nominal(NomTree,Person,Number,Gender)), Number,Gender) ==>
	?(negation),
	det(Number,Gender,Stem1),
	nominal(NomTree,Person,Number,Gender).

% np--> det,qp,nominal
% Noun phrase with quantifiers
% the two books
np((det(det(Number,Gender,Stem1)),qp(Number,Stem2),nominal(NomTree)), Number,Gender) ==>
	?(negation),
	det(Number,Gender.Stem1),
	qp(Number,Stem2),
	nominal(NomTree, _, Number,Gender).

% np --> pronoun
% Singleton pronouns:
% he
% Mr Wilson
np((pronoun(Number,Person,Case,Gender)),Number,Person,Gender) ==> 
	pronoun(Number,Person,Case,Gender).

% np --> np, conjunction, np.
% Conjoined noun-phrases. The are always plural.
% Men and boys	 : Good question.. Should gender be masc?
% You or them	 : Gender=none
% Fire and water : Person=none,Gender=noune
% * Fire and you
% us and them		- damnit (second and third person, so person might not match).
np((np(NPTree1,Number1,Person1,Gender1),conjunction(CStem),np(NPTree2,Number2,Person2,Gender2)),plur,Person,Gender) ==>
	np(NPTree1,Number1,Person1,Gender1),
	conjunction(CStem),
	np(NPTree2,Number2,Person2,Gender2).

% np --> ap, nominal.
% Adjective phrase and nominal.
% Ex: 
% Red carpet.
% Small, brown, nosy mouse.
np((ap(APTree), nominal(NomTree,Countable,Number,Gender)),Countable,Number,Person,Gender) ==>
	ap(APTree),
	nominal(NomTree,Countable,Person,Number,Gender).

% np --> det, ap, nominal
np(Number) ==>
	det(Number),
	ap(APTree),
	nominal(Number).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% qp: Combinations of quantifiers, cardinals and nomimals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
qp(ordinal(Number,Stem),Number) ==> ordinal(Number,Stem).
qp(cardinal(Number,Stem),Number) ==> cardinal(Number,Stem).
qp(quantifier(Number,Stem),Number) ==> quantifier(Number,Stem).

% Combinations. We allow them all, but they most agree in number.
qp(Number) ==> qp(Number), qp(Number).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ap: Adjective phrases
% An adjectival phrase or adjective phrase (AP) is a phrase with 
% an adjective as its head. Adjectival phrases may occur as pre- or 
% postmodifiers to a noun, or as predicatives (predicate adjectives)
% to a verb (e.g. full in the bottle is full).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ap(adjective(Stem)) ==>
	adjective(Stem).

ap((adjective(Stem1),comma(Stem2),adjective(Stem3))) ==>
	adjective(Stem1),
	comma(Stem2),
	adjective(Stem3).

ap((adjective(Stem1),comma(Stem2),ap(Tree))) ==>
	adjective(Stem1),
	comma(Stem2),
	ap(Tree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Nominals
% Features (in order):
% Tree: The parse tree built
% Countable: The countability. Values are countable/mass.
% Number: sing/plur
% Gender: masc/fem/neut
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The simplest type: Just a noun.
nominal(noun(Countable,Number,Gender,Stem),Countable,Number,Gender) ==>
	noun(Countable,Number,Gender,Stem).

% Compound nominals. 
% The compound nominal takes features from the last noun.
% Examples:
% city airport.
% company policy.
% water snakes.
% Syntax error.
% peoples' right. (i think these should be in a different "possive nominal rule")
% employees' right.
% Not sure about number agreement here. For instance "the executives" Probably search brown for to nouns in a row. It seems though
% that it goes: [sing,sing] or [sing,plur] 
nominal((noun(Countable1,Number1,Gender1,Stem1),nominal(NomTree,Countable2,Number2,Gender2)), Countable2,Number2,Gender2) ==>
	noun(Contable1,Number1,Gender1,Stem1),
	nominal(NomTree,Countable2,Number2,Gender2).

%% Some nouns have post-modifiers:
% Nominal with preposition postmodifier:
% Ex: the weather in April
% I am not sure about agreement for this one? Seems everything is ok.
% Cows in the staple
% Cows in the staple
% Cow in the boxes.
% Smoke on the water (mass nouns)
% Water in the pipe.
nominal((nominal(NomTree,Countable1,Number1,Gender1),pp(PPTree,Countable2,Number2,Gender2)),Countable1,Number1, Gender1) ==>
	nominal(NomTree,Countable1,Number1,Gender1),
	pp(PPTree,Countable2,Number2,Gender2),
	{}. % Leave agreement for later.
	
% Nominal with gerundive postmodifier.
nominal((nominal(NomTree,Countable,Number,Gender),gerund_vp(VPTree)), Countable, Number, Gender) ==>
	nominal(NomTree,Countable,Number,Gender),
	gerund_vp(VPTre).

nominal((nominal(NomTree,Countable,Number,Gender),relative_clause(RCTree)),Countable,Number,Gender) ==>
	nominal(NomTree,Countable,Number,Gender),
	relative_clause(RCTree).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Relative clauses
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% We need some gap-list like functionality here.

relative_clause((relative_pronoun(Number,Person,Gender),vp()),) ==> 
	relative_pronoun(Number,Person,Gender),
	vp().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pp: Prepositional phrase
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% from Los Angeles
pp((preposition(Stem),nominal(NomTree,Countable,Number,Gender)),Countable,Number,Gender) ==>
	preposition(Stem),
	nominal(NomTree,Countable,Number,Gender).

% Note:
% We don't need extra rules to cover longer prepositional phrases like
% The girl in the red dress from the train to copenhagen from yesterday. (nevermind that is hopelessly ambiguous).
% since we already have: nominal ==> nominal, pp. and they are mutually recursive.