%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A lexicon based on the Brown corpus
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% tag: (
% Description: opening parenthesis 

brown_cardinal(Word) :-
	brown_pos_tag('cd',Word).

brown_negator(Word) :-
	brown_pos_tag('*',Word).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determiners
determiner(Word, Number, Person, Case, Gender, pre_qualifier) :-
	brown_pos_tag(Word,'abl'),
	all_number(Number),
	all_person(Person),
	all_case(Case),
	all_gender(Gender).

% all half many nary 
determiner(Word, Number, Person, Case, Gender, pre_quantifier) :-
	brown_pos_tag(Word,'abn'),
	all_number(Number),
	all_person(Person),
	all_case(Case).
	
determiner(Word, plural, Person, Case, Gender, conjuction) :-
	brown_pos_tag('abx',Word),
	all_person(Person),
	all_case(Case),
	all_gender(Gender).
	
% Some of these should only occur with plural/singular attachments. But there is no distinction in Brown.
ordinal(Word, Number, Person, Case, Gender,post_determiner):-
	brown_pos_tag('ap',Word),
	all_person(Person),
	all_case(Case),
	all_gender(Gender).

	setof(X,brown_tag_keyword(X,'determiner/pronoun'),X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Conjunctions	
conjunction(Word,coordinating) :-
	brown_pos_tag('cc',Word).
conjunctions(Word,subordinating) :-
	brown_pos_tag('cc',Word).