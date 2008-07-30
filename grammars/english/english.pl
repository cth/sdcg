% A relatively wide coverage english grammar originally based on mainly Jurafsky chap 9.
start ==> sentence.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% s: Sentence types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A sentence may end with a stop character like dot, but¨
% we do not require it 
sentence ==>
	sentence, stop.

% Simple, short sentences:
% Sometimes sentences are just a single word or phrase. 
% For instance the answer to a question, may be answered
% quite briefly. "Who did it? Him."
% Or a command being issued, "stop!".

sentence ==> np(Number,Person,Gender).
sentence ==> ap.
sentence ==> adverb_p.
sentence ==> qp(_).

% Basic np vp sentences
% Example: I prefer the morning flight
sentence ==>
	np(Number,Person,Gender),
	vp(Number,Tense,Person).

% Imperative structure verb sentences. Have no subject.
% Example: Show the lowest fare
sentence ==>
	vp(Number,Tense,Person).
	
sentence ==>
	np(Number,Person,Gender),
	sentence.
	
% On tuesday, it will happen.
sentence ==>
	pp(_countable,_number,_case),
	comma,
	sentence.

sentence ==>
	sentence,
	colon,
	sentence.

% Conjuction of sentences:
sentence ==>
	sentence,
	conjunction(_),
	sentence.

sentence ==>
	adverb(_,_),
	comma,
	sentence.

sentence ==>
	sentence,
	comma,
	adverb(_,_).

% Sentence with pre qualifiers
% So I cooked some food.
/*
sentence ==>
	qualifier(pre),
	sentence.

% Sentence with post qualifier
% It is strange, indeed.
sentence ==>
	sentence,
	qualifier(post).
*/	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question-like sentence constructions

% s --> aux,np,vp
% Auxillary verbs does not include do/does, but includes should,may,could etc.
% Example:
% must we go ?
% Should they decide how we proceed.
sentence ==>
	modal(Stem),
	np(Number,Person,Case),
	vp(Number,Tense,Person).

% s --> aux_verb, np, vp.
% Do/have behave quite differently in question like sentences.
% do: VP in present tense -- Do they stop?
% did: VP in present tense -- Did they think about it?
sentence ==>
	verb(VNumber,VTense,VCase,VPerson,to_do),
	np(Number,Person,Case),
	vp(Number,present,Person).

% have: VP in past tense --	Have any people come?, have you done it?
% had: VP in past tense -- Had they thought about it?
sentence ==>
	verb(VNumber,VTense,VCase,VPerson,to_have),
	np(Number,Person,Case),
	vp(Number,past,Person).

% s --> wh, np, vp.
% wh-subject-question
% Which flights serve breakfeast?
sentence ==>
	wh_determiner(Stem),
	np(Number,Person,Case),
	vp(Number,Tense,Person).

% s --> wh,np,verb('to do'/'to have'),np,vp
% wh-non-subject-question
% What flights do you have from A to B?
% What traits did he show?
% *How do you do?
sentence ==>
	wh_determiner(Stem),
	np(Number1,Person1,NPCase1),
	%verb(Number,VTense,Person1,VCase,to_do),
	verb(Number,VTense,Person1,VCase,_), 
	np(NPNumber2,NPPerson2,NPCase2),
	vp(VPNumber,present,VPPerson). % is always in present tense

% Which editor have you used
sentence ==>
	wh_determiner(Stem),
	np(Number1,Person1,NPCase1),
	%verb(Number1,VTense,Person1,VCase,to_do),
	verb(Number1,VTense,Person1,VCase,_),	
	% aux(Num2), % _^ 
	np(NPNumber2,NPPerson2,NPCase2),
	vp(VPNumber,past,VPPerson). % is always past tense

% A very simple account of fronting:
% On the table.
sentence ==>
	pp(_countable,_number,_case).
	
%s ==> start_enclose, s, end_enclose.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% vp: Verb phrases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features: vp(Tree,Number,Tense,Person)

% vp --> verb.
% It [sucks].
vp(Number,Tense,Person) ==>
	verb(Number,Tense,Person,VCase,Stem).
% vp --> modal, verb.
% It will happen.
vp(Number,Tense,Person) ==>
	modal(ModalStem),
	vp(Number,Tense,Person).
	
% vp --> verb, np.
% Number between the verb and np doesn't have to agree, but the vp takes it's number from the verb. This also goes for tense and person.
% Examples:
% Prefer a morning flight.
% Help yourself.
vp(VNumber,VTense,VPerson) ==>
	verb(VNumber,VTense,VPerson,VCase,VStem),
	np(NPPerson,NPNumber,objective).
vp(VNumber,VTense,VPerson) ==>
	verb(VNumber,VTense,VPerson,VCase,VStem),
	np(NPPerson,NPNumber,reflexive).
	
% vp --> verb, np, pp.
% Examples:
% Leave boston in the morning
% Read books on natural language in the evenings.
vp(VNumber,VTense,VPerson) ==>
	verb(VNumber,VTense,VPerson,VCase,VStem),
	np(NPPerson,NPNumber,NPCase),
	pp(PPCountable,PPNumber,PPCase).

% vp --> verb,pp.
% work on thursday
vp(VNumber,VTense,VPerson) ==>
	verb(VNumber,VTense,VPerson,VCase,VStem),
	pp(PPCountable,PPNumber,PPCase).

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
vp(Number,Tense1,Person) ==>
	vp(Number,Tense1,Person),
	conjunction(_),
	vp(Number,Tense2,Person).
vp(Number,Tense2,Person) ==>
	vp(Number,Tense1,Person),
	conjunction(_),
	vp(Number,Tense2,Person).
	
% vp --> verb("to be" or to "have")
% was modernizing
% is/are modernizing
% were modernizing
% -- There might be a problem with infinitive forms of "to be". Depends on how I decide to represent infinitives.
vp(Number,Tense,Person) ==>
	verb(Number,Tense,Person,VCase,to_be),
	gerund_vp.

% vp --> verb('to be'),verb('to have'), gerund_vp
% has been modernizing: Tense is "present perfect"
% had been modernizing: Tense is "past perfect"
% *had modernizing
vp(Number,Tense,Person) ==>
	verb(Number,Tense,Person,VCase,to_have),
	verb(Number1,past,Person,particple,to_be), % been
	gerund_vp.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gerund verbs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gerund_vp ==>
	gerund_verb(Stem).

gerund_vp ==> 
	gerund_verb(Stem),
	np(NPPerson,NPNumber,NPCase).
	
gerund_vp ==>
	gerund_verb(Stem),
	pp(PPCountable,PPNumber,PPCase).
	
gerund_vp ==>
	gerund_verb(Stem),
	np(Number,Person,Gender),
	pp(PPCountable,PPNumber,PPCase).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% np: Noun phrases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features:
% - Number: sing/plur
% - Person: first,second,third
% - Case: 
%	- We really only care if np's can occur in subject or object position
%  
% Questions to self: Should np have person? - yes, since they can contain pronouns
% Then, can we assume all common are third person? - They are in brown, so yes.

% np ==> (det) (card) (ord) (quant) (AP) Nominal
	
% Pronoun nominal
%np(Number,Person,Case) ==> 
%	pronoun(Number,Person,Case,Gender,_).

% reflexive should occur in subject only
np(Number,Person,subjective) ==>
	pronoun(Number,Person,reflexive,_gender,_).
np(Number,Person,subjective) ==>
	pronoun(Number,Person,subjective,_gender,_).

% Her mood	
% His third try
np(Number,Person,@subj_obj(Case)) ==>
	pronoun(_number,_person,possesive-determiner,_gender,_whatever),
	nominal(Person,Number,_case).
np(Number,Person,@subj_obj(Case)) ==>
	pronoun(_number,_person,possesive-determiner,_gender,_whatever),
	qp(Number),
	nominal(Person,Number,_case).	
	
% np --> nominal
% Many nouns doesn't require a determiner: 
% Mass nouns (snow,dinner,water etc.)
% Plural nouns:  bunnies are funny.
% Singular nouns is not so easy: *bunny is funny (This is like baby talk)
% We can probably, make a rule for this in the grammar corrector
% np --> det, nominal
% The car
% The two cars
np(Number,Person,@subj_obj(Case)) ==>
	nominal(Person,Number,uninflicted).
	
np(Number,Person,@subj_obj(Case)) ==>
	det(Number,_),
	nominal(Person,Number,uninflicted).
	
np(Number,Person,@subj_obj(Case)) ==>
	qp(Number),
	nominal(Person,Number,uninflicted).
	
np(Number,Person,@subj_obj(Case)) ==>
	det(Number,_),
	qp(Number),	
	nominal(Person,Number,uninflicted).	

% The coach's decision
/*
np(Number,Person,@subj_obj(Case)) ==>
	?(det(Number1,_)),
	?(qp(Number1)),
	nominal(_person,Number1,genitive),
	np(Number),
	% No article may follow this
	nominal(Person,Number,uninflicted).
*/
np(Number,Person,@subj_obj(Case)) ==>
	nominal(_person,Number1,genitive),
	% No article may follow this
	nominal(Person,Number,uninflicted).
np(Number,Person,@subj_obj(Case)) ==>
	det(Number1,_),
	nominal(_person,Number1,genitive),
	% No article may follow this
	nominal(Person,Number,uninflicted).
np(Number,Person,@subj_obj(Case)) ==>
	qp(Number1),
	nominal(_person,Number1,genitive),
	% No article may follow this
	nominal(Person,Number,uninflicted).
np(Number,Person,@subj_obj(Case)) ==>
	det(Number1,_),
	qp(Number1),
	nominal(_person,Number1,genitive),
	% No article may follow this
	nominal(Person,Number,uninflicted).			

% Compound nouns
% The hotel owner
% The hotels owner
np(Number,third,Case) ==>
	np(_number,third,_case), 
	nominal(Person,Number,Case).
	
%%% NP conjunctions %%%% 
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
np(plur,Person,@subj_obj(Case)) ==>
	np(Number1,third,Gender1),
	conjunction(_),
	np(Number2,third,Gender2).

% Austin, Texas.
np(plur,Person,@subj_obj(Case)) ==>
	np(Number1,Person1,Gender1),
	comma,
	np(Number2,Person1,Gender2).
	
% np --> ap, nominal.
% Adjective phrase and nominal.
% Ex: 
% Red carpet.
% Small, brown, nosy mouse.
% long, curly har
/*
np(Number,third,Case) ==>
	ap,
	nominal(Countable,Number,Case).
	
% np --> det, ap, nominal
% The pretty girl
np(Number,third,Case) ==>
	det(Number,_),
	ap,
	nominal(Countable,Number,Case).
	
% Underlying problem
np(Number,third,Case) ==>
	adverb_p,
	nominal(Countable,Number,Case).

% The underlying problem
np(Number,third,Case) ==>
	det(Number,_),
	nominal(Countable,Number,Case).
*/

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

/* 
Next four rules can be implemented with this but regex dont work with append
nominal(Countable,Number,Case) ==>
	?(adverb_p),
	?(ap),
	nominal(Countable,Number,Case).
*/

% Underlying problem
% Surprisingly calm exterior	
nominal(Countable,Number,Case) ==>
	adverb_p,
	nominal(Countable,Number,Case).
	
nominal(Countable,Number,Case) ==>
	ap,
	nominal(Countable,Number,Case).
	
nominal(Countable,Number,Case) ==>
	adverb_p,
	ap,
	nominal(Countable,Number,Case).

% nominal --> noun 
% The simplest type: Just a noun.
nominal(Countable,Number,Case) ==>
	noun(Countable,Number,Case,_).
	
nominal(Countable,Number,Case) ==>
	propernoun(Countable,Number,Case,_).

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
nominal(Countable2,Number2,uninflicted) ==>
	noun(Countable1,Number1,Case,Stem1),
	nominal(Countable2,Number2,uninflicted).
	
nominal(Countable2,Number2,uninflicted) ==>
	propernoun(Countable1,Number1,Case,Stem1),
	nominal(Countable2,Number2,uninflicted).	

%% Some nouns have post-modifiers:
% Nominal with preposition postmodifier:
% Ex: the weather in April
% I am not sure about agreement for this one? Seems everything is ok.
% Cows in the staple
% Cows in the staple
% Cow in the boxes.
% Smoke on the water (mass nouns)
% Water in the pipe.
nominal(Countable1,Number1,Case) ==>
	nominal(Countable1,Number1,uninflicted),
	pp(Countable2,Number2,uninflicted).
	% Leave agreement for later.
	
% Nominal with gerundive postmodifier.
% tree cutting
% computer programming
% This should not be allowed with pronouns!!!
nominal(Countable, Number,uninflicted) ==>
	nominal(Countable,Number,uninflicted),
	gerund_vp.

% nominal --> nominal, relative_clause
% Nominals occuring with relative clauses:
% The boy who sings.
nominal(Countable,Number,uninflicted) ==>
	nominal(Countable,Number,uninflicted),
	relative_clause(Number,_Person,_Tense).
	
%nominal(Countable,Number,Gender) ==>
%	adverb_p,
%	nominal(Countable,Number,Gender).	
	
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
relative_clause(Number,Person,Tense) ==>
	% We should match gender, but brown corpus makes it impossible
	relative_pronoun(accusative,_Gender,Stem), % that,who etc.
	vp(Number,Tense,Person).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pp: Prepositional phrase
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Where are you from.
pp(Countable,Number,Case) ==>
	preposition(_).

% from Los Angeles
pp(Countable,Number,Case) ==>
	preposition(_),
	nominal(Countable,Number,Case).
	
% Note:
% We don't need extra rules to cover longer prepositional phrases like
% The girl in the red dress from the train to copenhagen from yesterday. (nevermind that is hopelessly ambiguous).
% since we already have: nominal ==> nominal, pp. and they are mutually recursive.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% qp: Combinations of quantifiers, cardinals and nomimals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
qp(Number) ==> 
	ordinal(Number,_).
	
qp(Number) ==>
	cardinal(Number,_).
	
qp(Number) ==>
	quantifier(Number,_).

% Combinations. We allow them all, but they most agree in number.
qp(Number) ==>
	qp(Number),
	qp(Number).
	
% Above rule can be expressed as
%qp(Number) ==>
%	*(qp(Number)),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ap: Adjective phrases
% An adjectival phrase or adjective phrase (AP) is a phrase with 
% an adjective as its head. Adjectival phrases may occur as pre- or 
% postmodifiers to a noun, or as predicatives (predicate adjectives)
% to a verb (e.g. full in the bottle is full).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ap ==> adjective(_).

ap ==>
	adjective(_),
	comma,
	adjective(_).

ap ==>
	adjective(_),
	comma,
	ap.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adverb phrases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Not really properly done yet

adverb_p ==>
	adverb(_,_).
	
% Repeated conjunctions of adverb phrases
% Surprisingly and 
adverb_p ==>
	adverb_p,
	conjunction(_),
	adverb_p.

adverb_p ==>
	adverb_p,
	comma,
	adverb_p.
	

% Most likely
adverb_p ==>
	quantifier(_,_), 
	adverb_p.
