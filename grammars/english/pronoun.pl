% Pronouns
% Features are: Number, Person, Case, Gender
% Values for Number: sg,pl,interogative
% Values for Person: first,second,third
% Values for Case: subjective, objective, genitive-determiner, genitive-nomimal, reflexive.

number(pl).
number(sg).

gender(male).
gender(female).
gender(nonhuman).

genitive(genitive-determiner).
genitive(genitive-nominal).

subj_obj(subjective).
subj_obj(objective).

pronoun(sg,first,subjective,@gender(G)) ==> [ 'I' ].
pronoun(sg,first,objective,@gender(G)) ==> [ 'me' ].
pronoun(sg,first,genitive-determiner,@gender(G)) ==> [ 'my' ].
pronoun(sg,first,genitive-nominal‚@gender(G)) ==> [ 'mine' ].

pronoun(pl,first,subjective,@gender(G)) ==> [ 'we' ].
pronoun(pl,first,objective,@gender(G)) ==> [ 'us' ].
pronoun(pl,first,genitive-determiner,@gender(G)) ==> [ 'our' ].
pronoun(pl,first,genitive-nominal,@gender(G)) ==> [ 'ours' ].

pronoun(@number(N),second,subjective,@gender(G)) ==> [ 'we' ].
pronoun(@number(N),second,objective,@gender(G)) ==> [ 'us' ].
pronoun(@number(N),second,genitive-determiner,@gender(G)) ==> [ 'our' ].
pronoun(@number(N),second,genitive-nominal,@gender(G)) ==> [ 'ours' ].

pronoun(sg,third,subjective,male) ==> [ 'he' ].
pronoun(sg,third,objective,male) ==> [ 'him' ].
pronoun(sg,third,@genitive(C),male) ==> [ 'his' ].

pronoun(sg,third,subjective,female) ==> [ 'she' ].
pronoun(sg,third,objective,female) ==> [ 'her' ].
pronoun(sg,third,genitive-determiner,female) ==> [ 'her' ].
pronoun(sg,third,genitive-nominal,female) ==> [ 'hers' ].

pronoun(pl,third,@subj_obj(C),nonhuman) ==> [ 'it' ].
pronoun(pl,third,@genitive(C),nonhuman) ==> [ 'its' ].

pronoun(pl,third,subjective,@gender(G)) ==> [ 'they' ].
pronoun(pl,third,objective,@gender(G)) ==> [ 'them' ].
pronoun(pl,third,genitive-determiner,@gender(G)) ==> [ 'their' ].
pronoun(pl,third,genitive-nominal,@gender(G)) ==> [ 'theirs' ].

pronoun(interogative,third,@subj_obj,@gender(G)) ==> [ 'who' ].
pronoun(interogative,third,objective,@gender(G)) ==> [ 'whom' ].
pronoun(interogative,third,genitive,@gender(G)) ==> [ 'whose' ].

pronoun(interogative,third,@subj_obj,@gender(G)) ==> [ 'who' ].
pronoun(interogative,third,objective,@gender(G)) ==> [ 'whom' ].
pronoun(interogative,third,genitive,@gender(G)) ==> [ 'whose' ].

pronoun(sg,first,reflexive,@gender(G)) ==> [ 'myself' ].
pronoun(pl,first,reflexive,@gender(G)) ==> [ 'ourselves' ].
pronoun(sg,second,reflexive,@gender(G)) ==> [ 'yourself' ].
pronoun(pl,second,reflexive,@gender(G)) ==> [ 'yourselves' ].

pronoun(sg,third,reflexive,male) ==> [ 'himself' ].
pronoun(sg,third,reflexive,female) ==> [ 'herself' ].
pronoun(sg,third,reflexive,nonhuman) ==> [ 'itself' ].
pronoun(pl,third,reflexive,@gender(G)) ==> [ 'themselves' ].