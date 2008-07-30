%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Lexicon rules for integration of brown corpus
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Set this to yes if you want to account for the 
% probability of the preterminal rule
match_inline(no).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expand modes for lex are all empty since lex is used as 
% a contraint/guard on the rule expansions
expand_mode(lexmap(-,-)).
expand_mode(matchword(-,+,-)).
expand_mode(matchword_inline(-,+,-)).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Grammar rules for expanding the lexicon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adjective(@lexmap(Tag,adjective),Word) ==>
	@matchword(Tag,Body,Word).

det(@lexmap(Tag,determiner(Number)),Number,Word) ==>
	@matchword(Tag,Body,Word).
	
wh_determiner(@lexmap(Tag,wh_determiner),Word) ==>
	@matchword(Tag,Body,Word).

noun(@lexmap(Tag,noun(Countable,Number,Case)), Countable,Number,Case,Word) ==>
	@matchword(Tag,Body,Word).
	
propernoun(@lexmap(Tag,propernoun(Countable,Number,Case)), Countable,Number,Case,Word) ==>
	@matchword(Tag,Body,Word).	

cardinal(@lexmap(Tag,cardinal(Number)),Number,Word) ==>
	@matchword(Tag,Body,Word).

ordinal(@lexmap(Tag, ordinal(Number)), Number,Word) ==>
	@matchword(Tag,Body,Word).
	
% Quantifier can have any number
quantifier(@lexmap(Tag, quantifier(Number)),Number,Word) ==>
	@matchword(Tag,Body,Word).
	
conjunction(@lexmap(Tag, conjunction), Word) ==>
	@matchword(Tag,Body,Word).

preposition(@lexmap(Tag, preposition), Word) ==>
	@matchword(Tag,Body,Word).


pronoun(@lexmap(Tag, pronoun(Number,Person,Case,Gender)), Number,Person,Case,Gender,Word) ==>
	@matchword(Tag,Body,Word).

relative_pronoun(@lexmap(Tag,relative_pronoun(Case,Gender)), Case ,Gender, Word) ==>
	@matchword(Tag,Body,Word).

verb(@lexmap(Tag, verb(Number,Tense,Person,Case)), Number, Tense, Person, Case, Word) ==>
	@matchword(Tag,Body,Word).

gerund_verb(@lexmap(Tag,gerund), Word) ==>
	@matchword(Tag,Body,Word).

modal(@lexmap(Tag,modalverb), Word) ==>
	@matchword(Tag,Body,Word).
	
adverb(@lexmap(Tag,adverb(Case)),Case,Word) ==>
	@matchword(Tag,Body,Word).
	
qualifier(@lexmap(Tag,qualifier(Case)),Case,Word) ==>
	@matchword(Tag,Body,Word).

% Punctuation stuff
comma(@lexmap(Tag,comma)) ==>
	@matchword(Tag,Body,Word).
	
colon(@lexmap(Tag,colon)) ==>
	@matchword(Tag,Body,Word).

%startquote(@lexmap(Tag,startquote)) ==>
%	@matchword(Tag,Body,Word).
	
%endquote(@lexmap(Tag,endquote)) ==>
%	@matchword(Tag,Body,Word).

stop(@lexmap(Tag,dot)) ==>
	@matchword(Tag,Body,Word).