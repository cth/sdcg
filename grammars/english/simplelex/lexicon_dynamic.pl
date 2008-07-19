%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adjectives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lex(adjective,Word) :-
	enum([expensive,cheap,cheapest,nonstop,first,latest,other,direct],Word).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determiners
% Determiners have similar properties to pronouns. Some pronouns can also be be used as determiners (eg. possessive).
% Article: The use of "a" and "an" depends on the following noun. If it begins with an vowel sound, then it's "an" otherwise it's "a".
% I contemplating modelling this dynamically, by testing the the noun.. 
%
% Features:
% Number: sing/plur
% Gender: masc/fem/neut
lex(determiner,sing,Word) :-
	enum([a,an,this,that],Word).
lex(determiner,plur,Word) :-
	enum([the,these,those],Word).
lex(determiner,Number,Word) :-
	num(Number),
	enum([which,what,whatever,whichever],Word).
lex(wh_determiner,Word) :-
	enum([which,what,whatever,whichever],Word).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Nouns
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A problem with Brown is that we cannot get the base of
% a word!lex(noun,
% Need something for determining if a noun is a mass noun
% lex(noun,countable,Type,Number)

lex(noun,countable,sing,neut,company).
lex(noun,countable,plur,neut,companies).
lex(noun,countable,sing,neut,department).
lex(noun,countable,plur,neut,departments).
lex(noun,countable,sing,neut,person).
lex(noun,countable,plur,neut,persons).
lex(noun,countable,sing,masc,man).
lex(noun,countable,plur,masc,men).
lex(noun,countable,sing,fem,woman).
lex(noun,countable,plur,fem,women).
lex(noun,countable,sing,neut,employee).
lex(noun,countable,plur,neut,employees).
lex(noun,countable,sing,neut,dog).
lex(noun,countable,plur,neut,dogs).
lex(noun,countable,sing,neut,salary).
lex(noun,countable,plur,neut,salaries).
lex(noun,countable,sing,neut,position).
lex(noun,countable,plur,neut,positions).
lex(noun,countable,sing,neut,budget).
lex(noun,countable,plur,neut,budgets).
lex(noun,countable,sing,neut,computer).
lex(noun,countable,plur,neut,computers).
lex(noun,countable,plur,neut,service).
lex(noun,countable,plur,neut,services).
lex(noun,countable,sing,neut,boss).
lex(noun,countable,plur,neut,bosses).
% Some nouns have no sing form:
lex(noun,countable,plur,neut,people). % no sing
lex(noun,countable,plur,neut,goods). % no sing form
% Flights domain
lex(noun,countable,sing,neut,flight).
lex(noun,countable,plur,neut,flights).
lex(noun,countable,sing,neut,fare).
lex(noun,countable,sing,neut,fares).
lex(noun,countable,sing,neut,dollar).
lex(noun,countable,plur,neut,dollars).
lex(noun,countable,sing,neut,reservation).
lex(noun,countable,plur,neut,reservations).
lex(noun,countable,sing,neut,breeze).
lex(noun,countable,plur,neut,breeze).
% Mass nouns cannot be counted
lex(noun,mass,sing,neut,music).
lex(noun,mass,plur,neut,music).
lex(noun,mass,sing,neut,math).
lex(noun,mass,plur,neut,math).
lex(noun,mass,sing,neut,water).
lex(noun,mass,plur,neut,water).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cardinals
lex(cardinal,sing,one).

lex(cardinal,plur,Word) :-
	enum([one,two,three,four,five,six,seven,eight,nine,ten],Word).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ordinals

% Ordinals that may occur with any count
lex(ordinal,Number,Word) :-
	num(Number),
	enum([first,next,second,last,other],Word).

% Ordinals that may only occur with plural count nouns
lex(ordinal,plur,many).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Quantifier


lex(quantifier,Word) :-
	enum([first-class,nonstop,longest,earliest],Word).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Conjunctions

lex(conjunction,Word) :-
	enum([and,or,but],Word).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prepositions

lex(preposition,Word) :-
	enum([from,to,in,on,at,near,throughout],Word).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pronouns
% Features are: Number, Person, Case, Gender
% Values for Number: sing,plur,interogative
% Values for Person: first,second,third
% Values for Case: subjective, objective, possesive-determiner, possesive-nomimal, reflexive.
% I dont think pronouns can be countable!

% One rule to ring them all

% First-person singular:
lex(pronoun,sing,first,subjective,i).
lex(pronoun,sing,first,objective,G,me) :- gender(G).
lex(pronoun,sing,first,possesive-determiner,G,my) :- gender(G).
lex(pronoun,sing,first,possesive-nominal,G,mine) :- gender(G).

% first-person plural (we) - inclusive (you and I) and exclusive (someone else and I but not you)
lex(pronoun,plur,first,subjective,G,we) :- gender(G).
lex(pronoun,plur,first,objective,G,us) :- gender(G).
lex(pronoun,plur,first,possesive-determiner,G,our) :- gender(G).
lex(pronoun,plur,first,possesive-nominal,G,ours) :- gender(G).
% second-person singular or plural (you) - many English speakers amplify 
% the pronoun with following words such as "you all", "you guys", "you both", etc. to disambiguate singular/plural
lex(pronoun,N,second,subjective,G,you) :- num(N),gender(G).

lex(pronoun,sing,third,subjective,masc,he).
lex(pronoun,sing,third,objective,masc,him).
lex(pronoun,sing,third,C,masc,his) :- possesive(C).

lex(pronoun,sing,third,subjective,fem,she).
lex(pronoun,sing,third,objective,fem,her).
lex(pronoun,sing,third,possesive-determiner,fem,her).
lex(pronoun,sing,third,possesive-nominal,fem,hers).

lex(pronoun,plur,third,C,neut,it) :- subj_obj(C).
lex(pronoun,plur,third,C,neut,its) :- possesive(C).

lex(pronoun,plur,third,subjective,G,they) :- gender(G).
lex(pronoun,plur,third,objective,G,them) :- gender(G).

lex(pronoun,plur,third,possesive-determiner,G,their) :- gender(G).
lex(pronoun,plur,third,possesive-nominal,G,theirs) :- gender(G).

lex(pronoun,sing,first,reflexive,G,myself) :- gender(G).
lex(pronoun,plur,first,reflexive,G,ourselves) :- gender(G).
lex(pronoun,sing,second,reflexive,G,yourself) :- gender(G).
lex(pronoun,plur,second,reflexive,G,yourselves) :- gender(G).

lex(pronoun,sing,third,reflexive,masc,himself).
lex(pronoun,sing,third,reflexive,fem,herself).
lex(pronoun,sing,third,reflexive,neut,itself).
lex(pronoun,plur,third,reflexive,G,themselves) :- gender(G).

% Some places
lex(pronoun,sing,third,Case,neut,Word) :-
	subj_obj(Case),
	enum([alaska,copenhagen, newYork],Word).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Relative pronouns
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WH-pronoun, accusative 
% It may occur in sentences like: "The dog _that_ bites".
%lex(relative_pronoun,Number,third,neut,that) :-
%	num(Number).
%lex(relative_pronoun,Number,third,Gender,who) :-
%	human(Gender),
%	num(Number).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Relative pronouns
% Case: nominative/accusative/genitive
% Gender: masc/fem/neut
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Accusative: (brown tag WPO)
lex(relative_pronoun,accusative,Gender,Word) :-
	enum([who,whom],Word),
	human(Gender).
lex(relative_pronoun,accusative,neut,Word) :-
	enum([that,what,which],Word).

%relative_pronoun(accusative,@human(_G),@enum([who,whom],Stem)) ==> [Stem].
%relative_pronoun(accusative,neut,@enum([that,what,which],Stem)) ==> [Stem].

% Nominative:
lex(relative_pronoun,nominative,Gender,Word) :-
	enum([who,whoever, whosoever],Word),
	human(Gender).
lex(relative_pronoun,nominative,neut,Word) :-
	enum([that,what,whatever,whatsoever],Word).

%relative_pronoun(nominative,@human(_G),@enum([who,whoever, whosoever],Stem)) ==> [Stem].
%relative_pronoun(nominative,neut,@enum([that,what,whatever,whatsoever],Stem)) ==> [Stem].

% Genitive:
lex(relative_pronoun,genitive,Gender,Word) :-
	human(Gender),
	enum([whose,whosever],Word).
%relative_pronoun(genitive,@human(_G),@enum([whose,whosever],Stem)) ==> [Stem].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Verbs
% Tense:
% Number: sing/plur
% Tense: past, present, future
% Person: first, second, third
% Valency: intransitive, transitive, ditransitive

% English only shows distinctive agreement in the third person singular, present tense form of verbs 
% (which is marked by adding "-s"); the rest of the persons are not distinguished in the verb.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Common verbs, present tenses
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These verbs can be used with either singular

lex(verb,Number,Person,present,Word) :-
	exclude(person,second,Person),
	num(Number),
	enum(
		[investigate,find,act,follow,inure,achieve,reduce,take,
		remedy,set,distribute,realize,disable,feel,receive,continue,
		place,protect,eliminate,elaborate,work,permit,run,enter,force],
		Word).
	
lex(verb,singular,third,present,Word) :-
	enum(
		[deserves,believes,receives,takes,goes,expires,says,opposes,starts,
		permits,expects,thinks,faces,votes,teaches,holds,calls,fears,spends,
		collects,backs,eliminates,sets,flies,gives,seeks,reads],
		Word).

lex(verb,Number,Person,past,Word) :-
	enum(
		[said,produced,took,recommended,commented,urged,found,added,
		praised,charged,listed,became,announced,brought,attended,wanted,
		voted,defeated,received,got,stood,shot,scheduled,feared,promised],
		Word),
	person(Person),
	num(Number).
	
lex(verb,Number,Person,past-participle,Word) :-
	num(Number),
	person(Person),
	enum(
		[conducted,charged,won,received,studied,revised,operated,accepted,
		combined,experienced,recommended,effected,granted,seen,protected,adopted,
		retarded,notarized,selected,composed,gotten,printed],
		Word).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gerund verbs
% Gerund verbs have no additional features (other than begin gerund)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% VBG corresponds to_gerunds in Brown

lex(gerund_verb,Word) :-
	enum(
		[being,preferring,arriving,requiring,leaving,modernizing,improving,purchasing,
		lacking,enabling,pricing,keeping,getting,picking,entering,voting,warning,
		making,strengthening,setting,neighboring,attending,participating,moving],
		Word).

% From Brown. There is seems to_be no tense distinction :-(
% e.g. "will" and "would" are both under the same tag (MD)
% Note to_self: Find out how to word modals are represented in brown e.g. "ought to/have to/can leave/might play"
lex(modal_verb,Word) :-
	enum([should,may,might,will,would,must,can,could,shall,ought],Word).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adverbs 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lex(adverb,Word) :-
	enum([only, often, generally, also, nevertheless, upon, together, back, newly, no, likely,
		meanwhile, near, then, heavily, there, apparently, yet, outright, fully, aside, consistently,
		specifically, formally, ever, just], Word).
		
lex(comma,comma).
