adjective(Word) ==>
	{ lex(adjective,Word) },
	[Word].

det(@num(Number),@gender(Gender),Word) ==>
	{ lex(determiner,Number,Word) },
	[Word].

wh_det(@num(_Number),@gender(_Gender),Word) ==>
	{ lex(wh_determiner,Word) },
	[Word].

noun(@countable(C),@num(N),@gender(G),Word) ==> 
	{ lex(noun,C,N,G,Word) },
	[ Word ].

cardinal(@num(Number),Word) ==>
	{ lex(cardinal,Number,Word) },
	[Word].


ordinal(@num(Number),Word) ==>
	{ lex(ordinal,Number,Word) },
	[Word].

quantifier(@num(Number),Word) ==>
 	{ lex(quantifier,Word) },
	[Word].
	
conjunction(Word) ==>
	{lex(conjunction(Word))},
	[Word].

preposition(Word) ==>
	{ lex(preposition(Word)) },
	[Word].

pronoun(@num(Number),@person(Person),@case(Case),Word) ==>
	{ lex(pronoun,Number,Person,Case,Word) },
	[ Word ].

relative_pronoun(@case(C),@gender(G),Word) ==>
	{ lex(relative_pronoun,C,G,Word) },
	[Word].


verb(@num(Number),@tense(Tense),@person(Person),Word) ==>
	lex(verb,Number,Person,Tense,Word),
	[Word].

gerund_verb(Word) ==> 
	lex(gerund_verb,Word),
	[Word].

modal(Word) ==> 
	lex(modal_verb,Word).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxillaries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% special auxilary verbs
verb(@num(_Number),present,third,to_do) ==> [ does ].
verb(@num(_Number),present,@exclude(person,third,_Person),to_do) ==> [ do ].
verb(@num(_Number),past,@person(_Person),to_do) ==> [ did ].

verb(@num(_Number),present,third,to_have) ==> [ has ].
verb(@num(_Number),present,@exclude(person,third,_Person),to_have) ==> [ have ].
verb(@num(_Number),past,@person(_Person),to_have) ==> [ had ].

verb(@num(_Number),present,third,to_be) ==> [ is ].
verb(@num(_Number),present,@exclude(person,third,_Person),to_be) ==> [ have ].
verb(sing,past,@person(_Person),to_be) ==> [ was ].
verb(plural,past,@person(_Person),to_be) ==> [ were ].
