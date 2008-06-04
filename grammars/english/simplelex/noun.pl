%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Nouns
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A problem with Brown is that we cannot get the base of
% a word!

% Need something for determining if a noun is a mass noun
% noun(countable,Type,Number)

plur(company,companies).

noun(countable,sing,neut,'company') ==> ['company'].
noun(countable,plur,neut,'company') ==> ['companies'].
noun(countable,sing,neut,'department') ==> ['department'].
noun(countable,plur,neut,'department') ==> ['departments'].
noun(countable,sing,neut,'person') ==> ['person'].
noun(countable,plur,neut,'person') ==> ['persons'].

noun(countable,sing,masc,'man') ==> ['man'].
noun(countable,plur,masc,'man') ==> ['men'].
noun(countable,sing,fem,'woman') ==> ['woman'].
noun(countable,plur,fem,'woman') ==> ['women'].
noun(countable,sing,neut,'employee') ==> ['employee'].
noun(countable,plur,neut,'employee') ==> ['employees'].
noun(countable,sing,neut,'dog') ==> ['dog'].
noun(countable,plur,neut,'dog') ==> ['dogs'].
noun(countable,sing,neut,'salary') ==> ['salary'].
noun(countable,plur,neut,'salary') ==> ['salaries'].
noun(countable,sing,neut,'position') ==> ['position'].
noun(countable,plur,neut,'position') ==> ['positions'].
noun(countable,sing,neut,'office_clerk') ==> ['office clerk'].
noun(countable,plur,neut,'office_clerk') ==> ['office clerks'].
noun(countable,sing,neut,'sales_rep') ==> ['sales representative'].
noun(countable,plur,neut,'sales_rep') ==> ['sales representatives'].
noun(countable,sing,neut,'budget') ==> ['budget'].
noun(countable,plur,neut,'budget') ==> ['budgets'].
noun(countable,sing,neut,'computer') ==> ['computer'].
noun(countable,plur,neut,'computer') ==> ['computers'].

noun(countable,plur,neut,'service') ==> ['service'].
noun(countable,plur,neut,'service') ==> ['services'].
noun(countable,sing,neut,'boss') ==> ['boss'].
noun(countable,plur,neut,'bosses') ==> ['bosses'].

% Some nouns have no sing form:
noun(countable,plur,neut,'people') ==> ['people']. % no sing
noun(countable,plur,neut,'goods') ==> ['goods']. % no sing form

% Mass nouns cannot be counted
noun(mass,sing,neut,'music') ==> ['music'].
noun(mass,plur,neut,'music') ==> ['music'].
noun(mass,sing,neut,'math') ==> ['math'].
noun(mass,plur,neut,'math') ==> ['math'].
noun(mass,sing,neut,'water') ==> ['water'].
noun(mass,plur,neut,'water') ==> ['water'].

% Flights domain
noun(countable,sing,neut,'flight') ==> ['flight'].
noun(countable,plur,neut,'flight') ==> ['flights'].
noun(countable,sing,neut,'fare') ==> ['fare'].
noun(countable,sing,neut,'fare') ==> ['fares'].
noun(countable,sing,neut,'dollar') ==> ['dollar'].
noun(countable,plur,neut,'dollar') ==> ['dollars'].
noun(countable,sing,neut,'reservation') ==> ['reservation'].
noun(countable,plur,neut,'reservation') ==> ['reservations'].
noun(countable,sing,neut,'breeze') ==> ['breeze'].
noun(countable,plur,neut,'breeze') ==> ['breeze'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Possive nouns
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

poss_noun(sing,company,'company\'s').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pronouns
% Features are: Number, Person, Case, Gender
% Values for Number: sing,plur,interogative
% Values for Person: first,second,third
% Values for Case: subjective, objective, possesive-determiner, possesive-nomimal, reflexive.
% I dont think pronouns can be countable!

% First-person singular:
pronoun(sing,first,subjective,@gender(G)) ==> [ 'I' ].
pronoun(sing,first,objective,@gender(G)) ==> [ 'me' ].
pronoun(sing,first,possesive-determiner,@gender(G)) ==> [ 'my' ].
pronoun(sing,first,possesive-nominal‚@gender(G)) ==> [ 'mine' ].

% first-person plural (we) - inclusive (you and I) and exclusive (someone else and I but not you)
pronoun(plur,first,subjective,@gender(G)) ==> [ 'we' ].
pronoun(plur,first,objective,@gender(G)) ==> [ 'us' ].
pronoun(plur,first,possesive-determiner,@gender(G)) ==> [ 'our' ].
pronoun(plur,first,possesivepossesive-determiner-nominal,@gender(G)) ==> [ 'ours' ].

% second-person singular or plural (you) - many English speakers amplify 
% the pronoun with following words such as "you all", "you guys", "you both", etc. to disambiguate singular/plural
pronoun(@number(N),second,subjective,@gender(G)) ==> [ 'you' ].

pronoun(sing,third,subjective,masc) ==> [ 'he' ].
pronoun(sing,third,objective,masc) ==> [ 'him' ].
pronoun(sing,third,@possesive(C),masc) ==> [ 'his' ].

pronoun(sing,third,subjective,femasc) ==> [ 'she' ].
pronoun(sing,third,objective,femasc) ==> [ 'her' ].
pronoun(sing,third,possesive-determiner,femasc) ==> [ 'her' ].
pronoun(sing,third,possesive-nominal,femasc) ==> [ 'hers' ].

pronoun(plur,third,@subj_obj(C),neut) ==> [ 'it' ].
pronoun(plur,third,@possesive(C),neut) ==> [ 'its' ].

pronoun(plur,third,subjective,@gender(G)) ==> [ 'they' ].
pronoun(plur,third,objective,@gender(G)) ==> [ 'them' ].
pronoun(plur,third,possesive-determiner,@gender(G)) ==> [ 'their' ].
pronoun(plur,third,possesive-nominal,@gender(G)) ==> [ 'theirs' ].

pronoun(sing,first,reflexive,@gender(G)) ==> [ 'myself' ].
pronoun(plur,first,reflexive,@gender(G)) ==> [ 'ourselves' ].
pronoun(sing,second,reflexive,@gender(G)) ==> [ 'yourself' ].
pronoun(plur,second,reflexive,@gender(G)) ==> [ 'yourselves' ].

pronoun(sing,third,reflexive,masc) ==> [ 'himself' ].
pronoun(sing,third,reflexive,fem) ==> [ 'herself' ].
pronoun(sing,third,reflexive,neut) ==> [ 'itself' ].
pronoun(plur,third,reflexive,@gender(G)) ==> [ 'themselves' ].

% These are not really pronouns are they? Anyway, they are _not_ countable!
pronoun(sing,third,@subj_obj(C),neut) ==> [ 'Alaska' ].
pronoun(sing,third,@subj_obj(C),neut) ==> [ 'Baltimore' ].
pronoun(sing,third,@subj_obj(C),neut) ==> [ 'Los Angeles' ].
pronoun(sing,third,@subj_obj(C),neut) ==> [ 'New York' ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Relative pronouns
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WH-pronoun, accusative 

relative_pronoun(@number(Number),third,neut) ==> ['that'].
relative_pronoun(@number(Number),third,@human(G)) ==> ['who'].

% We are going to have a separate wh-category
%pronoun(interogative,third,@subj_obj,@gender(G)) ==> [ 'who' ].
%pronoun(interogative,third,objective,@gender(G)) ==> [ 'whom' ].
%pronoun(interogative,third,genitive,@gender(G)) ==> [ 'whose' ].