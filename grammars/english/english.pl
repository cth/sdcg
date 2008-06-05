% A relatively wide coverage english grammar based on mainly Jurafsky chap 9.
% Note to self: Check if @ expansion can expand to a list of Unification symbols

sdcg ==> s(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% s: Sentence types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Basic np vp sentences
% Example: I prefer the morning flight
s(np(NPTree,Number,Person,Gender),vp(VPTree,Number,Tense,Person)) ==>
	np(NPTree,Number,Person,Gender),
	vp(VPTree,Number,Tense,Person).

% Imperative structure verb sentences. Have no subject.
% Example: Show the lowest fare
s(vp(Tree,Number,Tense,Person)) ==>
	vp(Tree,Number,Tense,Person).
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question-like sentence constructions

% s --> aux,np,vp
% Auxillary verbs does not include do/does, but includes should,may,could etc.
% Example:
% must we go ?
% Should they decide how we proceed.
s((aux(Stem),np(NPTree,Number,Person,Gender),vp(VPTree,Number,Tense,Person))) ==>
	modal(Stem),
	np(NPTree,Number,Person,Gender),
	vp(VPTree,Number,Tense,Person).

% s --> aux_verb, np, vp.
% Do/have behave quite differently in question like sentences.
% have: VP in past tense --	Have any people come?, have you done it?
% do: VP in present tense -- Do they stop?
% did: VP in present tense -- Did they think about it?
% had: VP in past tense -- Had they thought about it?
% Do any of these flights have stops? - I think we need to split this out: Eg. do/does
s((verb(VNumber,VTense,VPerson,'to do'),np(NPTree,Number,Person,Gender),vp(VPTree,Number,Tense,Person))) ==>
	verb(VNumber,VTense,VPerson,'to do')),
	np(NPTree,Number,Person,Gender),
	vp(VPTree,Number,present,Person).

s((verb(VNumber,VTense,VPerson,'to have'),np(NPTree,Number,Person,Gender),vp(VPTree,Number,Tense,Person))) ==>
	verb(VNumber,VTense,VPerson,'to have')),
	np(NPTree,Number,Person,Gender),
	vp(VPTree,Number,past,Person).
	
	
	
% s --> wh, np, vp.
% wh-subject-question
% Which flights serve breakfeast?
s((wh_determiner(Stem),np(NPTree,Number,Person,Gender),vp(VPTree,Number,Tense,Person))) ==> 
	wh_determiner(WHStem),
	np(NPTree,Number,Person,Gender),
	vp(VPTree,Number,Tense,Person).

% wh-non-subject-question
% What flights do you have from A to B.
% What traits did he have
% Which editor have you used
% *How do you do?
s ==> 
	wh(WHStem), 
	np(NPTree1,NPNumber1,NPPerson1,NPGender1),
	verb(VNumber,VTense,VPerson,@enum(['to have','to do'],Stem)),
	% aux(Num2), % _^ 
	np(NPTree2,NPNumber2,NPPerson2,NPGender2),
	vp(VPTree,VPNumber,present,VPPerson), 
	% We do explicit agreement for this one, as it is a bit complicated.
	{
		NPNumber1 == VNumber,
		NPPerson1 == VPerson,
	}.

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

% vp --> verb.
% It [sucks].
vp((verb(Number,Tense,Person,Stem)),Number,Tense,Person) :-
	verb(Number,Tense,Person,Stem).
	
% vp --> modal, verb.
% It will happen.
vp((modal(ModalStem),vp(Number,Tense,Person,VerbStem)),Number,Tense,Person) :-
	modal(ModalStem),
	vp(Number,Tense,Person,VerbStem).

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
	
% vp --> verb("to be" or to "have")
% was modernizing
% is/are modernizing
% were modernizing
% -- There might be a problem with infinitive forms of "to be". Depends on how I decide to represent infinitives.
vp((verb(Number,Tense,Person,Stem), gerund_vp(GVPTree)),Number,Tense,Person) ==>
	verb(Number,Tense,Person,'to be'),
	gerund_vp(GVPTree).

% vp --> verb('to be'),verb('to have'), gerund_vp
% has been modernizing: Tense is "present perfect"
% had been modernizing: Tense is "past perfect"
% *had modernizing
vp((verb(Number,Tense,Person,Stem), gerund_vp(GVPTree)),Number,Tense,Person) ==>
	verb(Number,Tense,Person,'to have'),
	verb(Number1,past-participle,Person,'to be'), % been
	gerund_vp(GVPTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gerund verbs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gerund_vp(gerund_verb(Stem),Stem) ==>
	gerund_verb(Stem).

gerund_vp((gerund_verb(Stem), np(NPTree,NPPerson,NPNumber,NPCase)),Stem) ==> 
	gerund_verb(Stem),
	np(NPTree,NPPerson,NPNumber,NPCase).
	
gerund_vp((gerund_Verb(Stem),pp(PPTree,PPCountable,PPNumber,PPCase)),Stem) ==>
	gerund_verb(Stem),
	pp(PPTree,PPCountable,PPNumber,PPCase).
	
gerund_vp((gerund_verb(Stem),np(NPTree,Number,Person,Gender), pp(PPTree,PPCountable,PPNumber,PPCase)),Stem) ==>
	gerund_verb(Stem),
	np(NPTree,Number,Person,Genders),
	pp(PPTree,PPCountable,PPNumber,PPCase).

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
% Many nouns doesn't require a determiner: 
% Mass nouns (snow,dinner,water etc.)
% Plural nouns:  bunnies are funny.
% Singular nouns is not so easy: *bunny is funny (This is like baby talk)
% We can probably, make a rule for this in the grammar corrector
np(nominal(NomTree,Person,Number,Case),third,Number,Case) ==>
	nominal(NomTree,Person,Number,Case).

% np --> det, nominal
np((det(det(Number,Gender,Stem1)),nominal(NomTree,Person,Number,Gender)),Number,Person,Gender) ==>
	det(Number,Gender,Stem1),
	nominal(NomTree,Person,Number,Gender).

% np--> det,qp,nominal
% Noun phrase with quantifiers
% the two books
np((det(det(Number,Gender,Stem1)),qp(Number,Stem2),nominal(NomTree)),Number,Person,Gender) ==>
	det(Number,Gender.Stem1),
	qp(Number,Stem2),
	nominal(NomTree, Person, Number,Gender).

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
% The pretty girl
np(det(Number,Gender,Stem),ap(APTree),nominal(NomTree,Countable,Number,Gender)) ==>
	det(Number,Gender,Stem),
	ap(APTree),
	nominal(NomTree,Countable,Number,Gender).

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
% Countable: countable/mass.
% Number: sing/plur
% Gender: masc/fem/neut
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A note for the person feature. Nominals never include pronouns
% so person is always 3rd. For this reason, this feature is
% omitted.

% nominal --> noun 
% The simplest type: Just a noun.
nominal(noun(Countable,Number,Gender,Stem),Countable,Number,Gender) ==>
	noun(Countable,Number,Gender,Stem).

% nominal --> noun, nominal
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
nominal((nominal(NomTree,Countable1,Number1,Gender1),pp(PPTree,Countable2,Number2,Gender2)),Countable1,Number1,Gender1) ==>
	nominal(NomTree,Countable1,Number1,Gender1),
	pp(PPTree,Countable2,Number2,Gender2).
	% Leave agreement for later.
	
% Nominal with gerundive postmodifier.
% tree cutting
% computer programming
% This should not be allowed with pronouns!!!
nominal((nominal(NomTree,Countable,Number,Gender),gerund_vp(VPTree)), Countable, Number, Gender) ==>
	nominal(NomTree,Countable,Number,Gender),
	gerund_vp(VPTre).

% nominal --> nominal, relative_clause
% Nominals occuring with relative clauses:
% The boy who sings.
nominal((nominal(NomTree,Countable,Number,Gender),relative_clause(RCTree)),Countable,Number,Gender) ==>
	nominal(NomTree,Countable,Number,Gender),
	relative_clause(RCTree,Number,Gender,Person,Tense).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Relative clauses
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% We need some gap-list like functionality here to handled multiple
% relative clause like: The man who likes the women who liked him.

% It's difficult to figure out agreement, consider these sentences:
% The boy who loves the girl.
% The boy whom the girl loves.
% We could have different rules for this, but Brown doesn't distiguish who/whom tagwise..

% The man who sings.
% The man who sing.
% The man who sang.
% The thing that works.
relative_clause((relative_pronoun(Number,Gender,Person),vp(Tree1,Number,Tense,Person)),Number,Gender,Person,Tense) ==>
	relative_pronoun(accusative,Gender), % that,who etc.
	vp(VPTree,Number,Tense,Person).
	
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