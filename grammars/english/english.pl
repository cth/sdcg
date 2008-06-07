% A relatively wide coverage english grammar based on mainly Jurafsky chap 9.
% Note to self: Check if @ expansion can expand to a list of Unification symbols

sdcg ==> s(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% s: Sentence types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Basic np vp sentences
% Example: I prefer the morning flight
s ==>
	np(NPTree,Number,Person,Gender),
	vp(VPTree,Number,Tense,Person).

% Imperative structure verb sentences. Have no subject.
% Example: Show the lowest fare
s ==>
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
% do: VP in present tense -- Do they stop?
% did: VP in present tense -- Did they think about it?
s((verb(VNumber,VTense,VPerson,to_do),np(NPTree,Number,Person,Gender),vp(VPTree,Number,present,Person))) ==>
	verb(VNumber,VTense,VPerson,to_do),
	np(NPTree,Number,Person,Gender),
	vp(VPTree,Number,present,Person).

% have: VP in past tense --	Have any people come?, have you done it?
% had: VP in past tense -- Had they thought about it?
s((verb(VNumber,VTense,VPerson,to_have),np(NPTree,Number,Person,Gender),vp(VPTree,Number,past,Person))) ==>
	verb(VNumber,VTense,VPerson,to_have),
	np(NPTree,Number,Person,Gender),
	vp(VPTree,Number,past,Person).

% s --> wh, np, vp.
% wh-subject-question
% Which flights serve breakfeast?
s((wh_determiner(Stem),np(NPTree,Number,Person,Gender),vp(VPTree,Number,Tense,Person))) ==>
	wh_determiner(Stem),
	np(NPTree,Number,Person,Gender),
	vp(VPTree,Number,Tense,Person).

% s --> wh,np,verb('to do'/'to have'),np,vp
% wh-non-subject-question
% What flights do you have from A to B?
% What traits did he show?
% *How do you do?
s((wh_determiner(Stem),np(NPTree1,NPNumber1,NPPerson1,NPGender1),verb(VNumber,VTense,VPerson,to_do),np(NPTree2,NPNumber2,NPPerson2,NPGender2),vp(VPTree,VPNumber,present,VPPerson))) ==>
	wh_determimer(Stem),
	np(NPTree1,NPNumber1,NPPerson1,NPGender1),
	verb(VNumber,VTense,VPerson,to_do),
	np(NPTree2,NPNumber2,NPPerson2,NPGender2),
	vp(VPTree,VPNumber,present,VPPerson), % is always in present tense
	% We do explicit agreement for this one, as it is a bit complicated.
	{
		% If NP is pronoun like 'he' (sing,2nd person) then the verb 'to be' must be 'does' (sing,2nd person).
		VNumber == NPNumber1,
		VPerson == NPPerson1
	}.

% Which editor have you used
s((wh_determiner(Stem),np(NPTree1,NPNumber1,NPPerson1,NPGender1),verb(VNumber,VTense,VPerson,to_do),np(NPTree2,NPNumber2,NPPerson2,NPGender2),vp(VPTree,VPNumber,present,VPPerson))) ==>
	wh_determiner(Stem),
	np(NPTree1,NPNumber1,NPPerson1,NPGender1),
	verb(VNumber,VTense,VPerson,to_do),
	% aux(Num2), % _^ 
	np(NPTree2,NPNumber2,NPPerson2,NPGender2),
	vp(VPTree,VPNumber,past,VPPerson), % is always past tense
	% We do explicit agreement for this one, as it is a bit complicated.
	{
		% If NP is pronoun like 'he' (sing,2nd person) then the verb 'to have' must be 'has' (sing,2nd person).
		VNumber == NPNumber1,
		VPerson == NPPerson1
	}.

% A very simple account of fronting:
% On the table.
s(pp(PPTree,PPCountable,PPNumber,PPGender)) ==>
	pp(PPTree,PPCountable,PPNumber,PPGender).

% On tuesday, it will happen.
s((pp(PPTree,PPCountable,PPNumber,PPGender),comma,s(STree))) ==>
	pp(PPTree,PPCountable,PPNumber,PPGender),
	comma,
	s(STree).

% Conjuction of sentences:
s((s(STree1),conjunction(Stem),s(STree2))) ==>
	s(STree1),
	conjunction(Stem),
	s(STree2).

%s ==> start_enclose, s, end_enclose.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% vp: Verb phrases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features: vp(Tree,Number,Tense,Person)

% vp --> verb.
% It [sucks].
vp((verb(Number,Tense,Person,Stem)),Number,Tense,Person) ==>
	verb(Number,Tense,Person,Stem).
	
% vp --> modal, verb.
% It will happen.
vp((modal(ModalStem),vp(Number,Tense,Person,VerbStem)),Number,Tense,Person) ==>
	modal(ModalStem),
	vp(Number,Tense,Person,VerbStem).

% vp --> verb, np.
% Number between the verb and np doesn't have to agree, but the vp takes it's number from the verb. This also goes for tense and person.
% Examples:
% Prefer a morning flight.
% Help yourself.
vp((verb(VNumber,VTense,VPerson,VStem),np(NPTree,NPPerson,NPNumber,objective)), VNumber,VTense,VPerson) ==>
	verb(VNumber,VTense,VPerson,VStem),
	np(NPTree,NPPerson,NPNumber,objective).
vp((verb(VNumber,VTense,VPerson,VStem),np(NPTree,NPPerson,NPNumber,reflexive)), VNumber,VTense,VPerson) ==>
	verb(VNumber,VTense,VPerson,VStem),
	np(NPTree,NPPerson,NPNumber,reflexive).
	
% vp --> verb, np, pp.
% Examples:
% Leave boston in the morning
% Read books on natural language in the evenings.
vp((verb(VNumber,VTense,VPerson,VStem),np(NPTree,NPPerson,NPNumber,NPCase),pp(PPTree,PPCountable,PPNumber,PPGender)), VNumber,VTense,VPerson) ==>
	verb(VNumber,VTense,VPerson,VStem),
	np(NPTree,NPPerson,NPNumber,NPCase),
	pp(PPTree,PPCountable,PPNumber,PPGender).

% vp --> verb,pp.
% work on thursday
vp((verb(VNumber,VTense,VPerson,VStem),pp(PPTree,PPCountable,PPNumber,PPGender)),VNumber,VTense,VPerson) ==>
	verb(VNumber,VTense,VPerson,VStem),
	pp(PPTree,PPCountable,PPNumber,PPGender).

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
vp((vp(Tree1,Number,Tense1,Person),conjunction(Stem),vp(Tree2,Number,Tense2,Person)),Number,Tense1,Person) ==>
	vp(Tree1,Number,Tense1,Person),
	conjunction(Stem),
	vp(Tree2,Number,Tense2,Person).
vp((vp(Tree1,Number,Tense1,Person),conjunction(Stem),vp(Tree2,Number,Tense2,Person)),Number,Tense2,Person) ==>
	vp(Tree1,Number,Tense1,Person),
	conjunction(Stem),
	vp(Tree2,Number,Tense2,Person).	
	
% vp --> verb("to be" or to "have")
% was modernizing
% is/are modernizing
% were modernizing
% -- There might be a problem with infinitive forms of "to be". Depends on how I decide to represent infinitives.
vp((verb(Number,Tense,Person,to_be), gerund_vp(GVPTree)),Number,Tense,Person) ==>
	verb(Number,Tense,Person,to_be),
	gerund_vp(GVPTree).

% vp --> verb('to be'),verb('to have'), gerund_vp
% has been modernizing: Tense is "present perfect"
% had been modernizing: Tense is "past perfect"
% *had modernizing
vp((verb(Number,Tense,Person,to_have), verb(Number1,past-participle,Person,to_be),gerund_vp(GVPTree)),Number,Tense,Person) ==>
	verb(Number,Tense,Person,to_have),
	verb(Number1,past-participle,Person,to_be), % been
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
	np(NPTree,Number,Person,Gender),
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
% Men and women	
% You or them	 : Gender=none
% Fire and water : Person=none,Gender=noune
% * Fire and you
% us and them		- damnit (second and third person, so person might not match).
% *he and she
% *we and they
% --> must be third person
np((np(NPTree1,Number1,Person,Gender1),conjunction(CStem),np(NPTree2,Number2,Person,Gender2)),plur,Person,neut) ==>
	np(NPTree1,Number1,Person,Gender1),
	conjunction(CStem),
	np(NPTree2,Number2,Person,Gender2),
	{
		Person == third
	}.

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
qp(ordinal(Number,Stem),Number) ==> 
	ordinal(Number,Stem).
	
qp(cardinal(Number,Stem),Number) ==>
	cardinal(Number,Stem).
	
qp(quantifier(Number,Stem),Number) ==>
	quantifier(Number,Stem).

% Combinations. We allow them all, but they most agree in number.
qp(Number) ==> 
	qp(Number),
	qp(Number).

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
	noun(Countable1,Number1,Gender1,Stem1),
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
	gerund_vp(VPTree).

% nominal --> nominal, relative_clause
% Nominals occuring with relative clauses:
% The boy who sings.
nominal((nominal(NomTree,Countable,Number,Gender),relative_clause(RCTree)),Countable,Number,Gender) ==>
	nominal(NomTree,Countable,Number,Gender),
	relative_clause(RCTree,Number,Gender,_Person,_Tense). % FIXME
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Relative clauses
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% We need some gap-list like functionality here to handled multiple
% relative clause like: The man who likes the women who liked him.

% It's difficult to figure out agreement, consider these sentences:
% The boy who loves the girl.
% The boy whom the girl loves.
% We could have different rules for this, but Brown doesn't distiguish who/whom tagwise..

% The man [who sings].
% The man [who sing].
% The man [who sang].
% The thing [that works].
relative_clause((relative_pronoun(Number,Gender,Stem),vp(VPTree,Number,Tense,Person)),Number,Gender,Person,Tense) ==>
	relative_pronoun(accusative,Gender,Stem), % that,who etc.
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