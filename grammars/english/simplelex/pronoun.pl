% Pronouns
% Features are: Number, Person, Case, Gender
% Values for Number: singular,plural,interogative
% Values for Person: first,second,third
% Values for Case: subjective, objective, genitive-determiner, genitive-nomimal, reflexive.

pronoun(singular,first,subjective,@gender(G)) ==> [ I ].
pronoun(singular,first,objective,@gender(G)) ==> [ me ].
pronoun(singular,first,genitive-determiner,@gender(G)) ==> [ my ].
pronoun(singular,first,genitive-nominal‚@gender(G)) ==> [ mine ].

pronoun(plural,first,subjective,@gender(G)) ==> [ we ].
pronoun(plural,first,objective,@gender(G)) ==> [ us ].
pronoun(plural,first,genitive-determiner,@gender(G)) ==> [ our ].
pronoun(plural,first,genitive-nominal,@gender(G)) ==> [ ours ].

pronoun(@number(N),second,subjective,@gender(G)) ==> [ we ].
pronoun(@number(N),second,objective,@gender(G)) ==> [ us ].
pronoun(@number(N),second,genitive-determiner,@gender(G)) ==> [ our ].
pronoun(@number(N),second,genitive-nominal,@gender(G)) ==> [ ours ].

pronoun(singular,third,subjective,male) ==> [ he ].
pronoun(singular,third,objective,male) ==> [ him ].
pronoun(singular,third,@genitive(C),male) ==> [ his ].

pronoun(singular,third,subjective,female) ==> [ she ].
pronoun(singular,third,objective,female) ==> [ her ].
pronoun(singular,third,genitive-determiner,female) ==> [ her ].
pronoun(singular,third,genitive-nominal,female) ==> [ hers ].

pronoun(plural,third,@subj_obj(C),nonhuman) ==> [ it ].
pronoun(plural,third,@genitive(C),nonhuman) ==> [ its ].

pronoun(plural,third,subjective,@gender(G)) ==> [ they ].
pronoun(plural,third,objective,@gender(G)) ==> [ them ].
pronoun(plural,third,genitive-determiner,@gender(G)) ==> [ their ].
pronoun(plural,third,genitive-nominal,@gender(G)) ==> [ theirs ].

pronoun(interogative,third,@subj_obj,@gender(G)) ==> [ who ].
pronoun(interogative,third,objective,@gender(G)) ==> [ whom ].
pronoun(interogative,third,genitive,@gender(G)) ==> [ whose ].

pronoun(interogative,third,@subj_obj,@gender(G)) ==> [ who ].
pronoun(interogative,third,objective,@gender(G)) ==> [ whom ].
pronoun(interogative,third,genitive,@gender(G)) ==> [ whose ].

pronoun(singular,first,reflexive,@gender(G)) ==> [ myself ].
pronoun(plural,first,reflexive,@gender(G)) ==> [ ourselves ].
pronoun(singular,second,reflexive,@gender(G)) ==> [ yourself ].
pronoun(plural,second,reflexive,@gender(G)) ==> [ yourselves ].

pronoun(singular,third,reflexive,male) ==> [ himself ].
pronoun(singular,third,reflexive,female) ==> [ herself ].
pronoun(singular,third,reflexive,nonhuman) ==> [ itself ].
pronoun(plural,third,reflexive,@gender(G)) ==> [ themselves ].

relative_pronoun(Number,Person,Gender) ==> [that].
relative_pronoun(Number,Person,Gender) ==> [who].

pronoun(singular,) ==> [alaska].
proper_noun ==> [baltimore].
proper_noun ==> [los,angeles].
proper_noun ==> [chicago].
proper_noun ==> [new,york].
