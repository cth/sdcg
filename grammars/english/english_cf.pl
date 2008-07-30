% A relatively wide coverage english grammar based on mainly Jurafsky chap 9.
% Note to self: Check if @ expansion can expand to a list of Unification symbols

start ==> sentence.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% s: Sentence types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Basic np vp sentences
% Example: I prefer the morning flight
sentence ==>
	np,vp.

% Imperative structure verb sentences. Have no subject.
% Example: Show the lowest fare
sentence ==>
	vp.
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question-like sentence constructions

% s --> aux,np,vp
% Auxillary verbs does not include do/does, but includes should,may,could etc.
% Example:
% must we go ?
% Should they decide how we proceed.
sentence ==>
	modal,
	np,
	vp.

% s --> aux_verb, np, vp.
% Do/have behave quite differently in question like sentences.
% do: VP in present tense -- Do they stop?
% did: VP in present tense -- Did they think about it?
sentence ==>
	verb,
	np,
	vp.

% have: VP in past tense --	Have any people come?, have you done it?
% had: VP in past tense -- Had they thought about it?
sentence ==>
	verb,
	np,
	vp.

% s --> wh, np, vp.
% wh-subject-question
% Which flights serve breakfeast?
sentence ==>
	wh_determiner,
	np,
	vp.

% s --> wh,np,verb('to do'/'to have'),np,vp
% wh-non-subject-question
% What flights do you have from A to B?
% What traits did he show?
% *How do you do?
sentence ==>
	wh_determimer,
	np,
	verb,
	np,
	vp.

% Which editor have you used
sentence ==>
	wh_determiner,
	np,
	verb,
	np,
	vp.
	
% A very simple account of fronting:
% On the table.
sentence ==>
	pp.

% On tuesday, it will happen.
sentence ==>
	pp,
	comma,
	sentence.

% Conjuction of sentences:
sentence ==>
	sentence,
	conjunction,
	sentence.

%s ==> start_enclose, s, end_enclose.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% vp: Verb phrases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features: vp(Tree,Number,Tense,Person)

% vp --> verb.
% It [sucks].
vp ==>
	verb.
	
% vp --> modal, verb.
% It will happen.
vp ==> 
	modal,
	vp.

% vp --> verb, np.
% Number between the verb and np doesn't have to agree, but the vp takes it's number from the verb. This also goes for tense and person.
% Examples:
% Prefer a morning flight.
% Help yourself.
vp ==>
	verb,
	np.
	
% vp --> verb, np, pp.
% Examples:
% Leave boston in the morning
% Read books on natural language in the evenings.
vp ==>
	verb,
	np,
	pp.

% vp --> verb,pp.
% work on thursday
vp ==>
	verb,
	pp.

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
vp ==>
	vp,
	conjunction,
	vp.
vp ==>
	vp,
	conjunction,
	vp.
	
% vp --> verb("to be" or to "have")
% was modernizing
% is/are modernizing
% were modernizing
% -- There might be a problem with infinitive forms of "to be". Depends on how I decide to represent infinitives.
vp ==>
	verb,
	gerund_vp.

% vp --> verb('to be'),verb('to have'), gerund_vp
% has been modernizing: Tense is "present perfect"
% had been modernizing: Tense is "past perfect"
% *had modernizing
vp ==>
	verb, % to have /
	verb, % to be
	gerund_vp.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gerund verbs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gerund_vp ==>
	gerund_verb.

gerund_vp ==> 
	gerund_verb,
	np.
	
gerund_vp ==>
	gerund_verb,
	pp.
	
gerund_vp ==>
	gerund_verb,
	np,
	pp.

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
np ==>
	nominal.

% np --> det, nominal
np ==>
	det,
	nominal.

% np--> det,qp,nominal
% Noun phrase with quantifiers
% the two books
np ==>
	det,
	qp,
	nominal.

% np --> pronoun
% Singleton pronouns:
% he
% Mr Wilson
np ==>
	pronoun.

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
np ==>
	np,
	conjunction,
	np.

% np --> ap, nominal.
% Adjective phrase and nominal.
% Ex: 
% Red carpet.
% Small, brown, nosy mouse.
np ==>
	ap,
	nominal.
	
% np --> det, ap, nominal
% The pretty girl
np ==>
	det,
	ap,
	nominal.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% qp: Combinations of quantifiers, cardinals and nomimals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
qp ==> 
	ordinal.
	
qp ==>
	cardinal.
	
qp ==>
	quantifier.

% Combinations. We allow them all, but they most agree in number.
qp ==>
	qp,
	qp.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ap: Adjective phrases
% An adjectival phrase or adjective phrase (AP) is a phrase with 
% an adjective as its head. Adjectival phrases may occur as pre- or 
% postmodifiers to a noun, or as predicatives (predicate adjectives)
% to a verb (e.g. full in the bottle is full).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ap ==> adjective.

ap ==>
	adjective,
	comma,
	adjective.

ap ==>
	adjective,
	comma,
	ap.

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
nominal ==>
	noun.

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
nominal ==>
	noun,
	nominal.

%% Some nouns have post-modifiers:
% Nominal with preposition postmodifier:
% Ex: the weather in April
% I am not sure about agreement for this one? Seems everything is ok.
% Cows in the staple
% Cows in the staple
% Cow in the boxes.
% Smoke on the water (mass nouns)
% Water in the pipe.
nominal ==>
	nominal,
	pp.
	% Leave agreement for later.
	
% Nominal with gerundive postmodifier.
% tree cutting
% computer programming
% This should not be allowed with pronouns!!!
nominal ==>
	nominal,
	gerund_vp.

% nominal --> nominal, relative_clause
% Nominals occuring with relative clauses:
% The boy who sings.
nominal ==>
	nominal,
	relative_clause.
	
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
relative_clause ==>
	%relative_pronoun(accusative,Gender,Stem), % that,who etc.
	relative_pronoun,
	vp.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pp: Prepositional phrase
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Where are you from.
pp ==>
	preposition.

% from Los Angeles
pp ==>
	preposition,
	nominal.
	
% Note:
% We don't need extra rules to cover longer prepositional phrases like
% The girl in the red dress from the train to copenhagen from yesterday. (nevermind that is hopelessly ambiguous).
% since we already have: nominal ==> nominal, pp. and they are mutually recursive.