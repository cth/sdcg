% fool compiler:
%start ==> s.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expand modes for lex are all empty since lex is used as 
% a contraint/guard on the rule expansions
expand_mode(lex(-,-)).
expand_mode(lex(-,-,-)).
expand_mode(lex(-,-,-,-)).
expand_mode(lex(-,-,-,-,-)).
expand_mode(lex(-,-,-,-,-,-)).
expand_mode(lex(-,-,-,-,-,-,-)). 

adjective(Word) ==>
	{ lex(adjective,Word) },
	[Word].

det(@lex(determiner,Number,_), Number,Word) ==>
	{ lex(determiner,Number,Word) },
	[Word].

wh_determiner(@lex(wh_determiner,Word),Word) ==>
	{ lex(wh_determiner,Word) },
	[Word].

noun(@lex(noun,Countable,Number,Gender,_), Countable,Number,Gender,Word) ==>
	{ lex(noun,Countable,Number,Gender,Word) },
	[ Word ].

cardinal(@lex(cardinal,Number,Word),Number,Word) ==>
	{ lex(cardinal,Number,Word) },
	[Word].

ordinal(@lex(ordinal,Number,Word), Number,Word) ==>
	{ lex(ordinal,Number,Word) },
	[Word].

% Quantifiers can have any number
quantifier(@num(Number),Word) ==>
 	{ lex(quantifier,Word) },
	[Word].

conjunction(Word) ==>
	{lex(conjunction,Word)},
	[Word].

preposition(Word) ==>
	{ lex(preposition,Word) },
	[Word].
pronoun(Number,Person,Case,Gender).

pronoun(@lex(pronoun,Number,Person,Case,Gender,_), Number,Person,Case,Gender,Word) ==>
	{ lex(pronoun,Number,Person,Case,Gender,Word) },
	[ Word ].

relative_pronoun(@lex(relative_pronoun,Case,Gender,_), Case ,Gender, Word) ==>
	{ lex(relative_pronoun,Case,Gender,Word) },
	[Word].

verb(@lex(verb,Number,Tense,Person,_), Number, Tense, Person, Word) ==>
	{ lex(verb,Number,Person,Tense,Word) },
	[Word].

gerund_verb(Word) ==> 
	{ lex(gerund_verb,Word) },
	[Word].

modal(Word) ==> 
	{ lex(modal_verb,Word) },
	[Word].
	
comma(Word) ==>
	{ lex(comma,Word) },
	[Word].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxillaries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% special auxilary verbs
% There is a problem listing these as normal verbs, since they
% are selected stochastically then.
verb(@num(_Number),present,third,to_do) ==>[ does ].
verb(@num(_Number),present,@exclude(person,third,_Person),to_do) ==> [ do ].
verb(@num(_Number),past,@person(_Person),Word) ==> [ did ].

verb(@num(_Number),present,third,to_have) ==> [ has ].
verb(@num(_Number),present,@exclude(person,third,_Person),to_have) ==> [ have ].
verb(@num(_Number),past,@person(_Person),to_have) ==> [ had ].

verb(@num(_Number),present,third,to_be) ==> [ is ].
verb(@num(_Number),present,@exclude(person,third,_Person),to_be) ==> [ have ].
verb(sing,past,@person(_Person),to_be) ==> [ was ].
verb(plural,past,@person(_Person),to_be) ==> [ were ].
