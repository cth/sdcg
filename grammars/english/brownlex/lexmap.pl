% lexmap/2:
% A mapping between the part of speech tags in the brown corpus and the terminals of the grammar

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The mapping from Brown tags to lexicon 
% categories
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Verbs
% Features: Number, Tense, Person, Form,
lexmap(vb, verb(sing, present,Person,uninflicted)) :-
	person(Person).
lexmap(vbz, verb(Number,past,Person,uninflicted)) :-
	num(Number),
	person(Person).
lexmap(vbg, verb(Number,present,Person,particple)) :-
	num(Number),
	person(Person).
lexmap(vbg, verb(Number,present,Person,gerund)) :-
	num(Number),
	person(Person).
lexmap(vbn, verb(Number,past,Person,partiple)) :-
	num(Number), person(Person).

lexmap(vbz, verb(sing,present,third,uninflicted)).
lexmap(vbd, verb(sing,past,third,uninflicted)).

% to have
lexmap(hv, verb(sing,present,first,uninflicted)).
lexmap(hv, verb(sing,present,second,uninflicted)).
lexmap(hv-negation, verb(sing,present,first,uninflicted)).
lexmap(hv-negation, verb(sing,present,second,uninflicted)).
lexmap(hvd, verb(sing,past,Person,uninflicted)) :-
	person(Person).
lexmap(hvd-negation, verb(sing,past,Person,uninflicted)) :-
	person(Person).
lexmap(hvn,verb(Number,past,Person,participle)) :-
	num(Number),
	person(Person).
lexmap(hvn-negation,verb(Number,past,Person,participle)) :-
	num(Number),
	person(Person).
lexmap(hvz,verb(sing,present,third,uninflicted)).
lexmap(hvz-negated,verb(sing,present,third,uninflicted)).
% to be
lexmap(be, verb(sing,present,first,uninflicted)).
lexmap(be, verb(sing,present,second,uninflicted)).
lexmap(bed, verb(sing,past,second,uninflicted)).
lexmap(bed, verb(plur,past,Person,uninflicted)) :-
	person(Person).
lexmap(bed-negated, verb(sing,past,second,uninflicted)).
lexmap(bed-negated, verb(plur,past,Person,uninflicted)) :-
	person(Person).
lexmap(bedz,verb(sing,past,first,uninflicted)).
lexmap(bedz,verb(sing,past,third,uninflicted)).
lexmap(bedz-negated,verb(sing,past,first,uninflicted)).
lexmap(bedz-negated,verb(sing,past,third,uninflicted)).
lexmap(bem, verb(sing,present,first,uninflicted)).
lexmap(bem-negated, verb(sing,present,first,uninflicted)).
lexmap(ben, verb(Number,past,Person,participle)) :-
	num(Number),
	person(Person).
lexmap(ber, verb(sing,present,second,uninflicted)).
lexmap(ber, verb(plur,present,Person,uninflicted)) :-
	person(Person).
lexmap(ber-negated, verb(sing,present,second,uninflicted)).
lexmap(ber-negated, verb(plur,present,Person,uninflicted)) :-
	person(Person).
lexmap(bez, verb(sing,present,third,uninflicted)).
lexmap(bez-negated, verb(sing,present,third,uninflicted)).
% to do
lexmap(do, verb(sing,present,first,uninflicted)).
lexmap(do-negated, verb(sing,present,first,uninflicted)).
lexmap(dod, verb(sing,past,second,uninflicted)).
lexmap(dod-negated, verb(sing,past,second,uninflicted)).
lexmap(doz, verb(sing,present,third,uninflicted)).
lexmap(doz-negated, verb(sing,present,third,uninflicted)).

% Gerund verbs
lexmap(beg,gerund).
lexmap(hvg,gerund).
lexmap(vbg,gerund).

% Modal verbs
lexmap(md, modalverb).
lexmap(md-negated, modalverb).

% Adjectives, no features
lexmap(jj, adjective).
lexmap(jj+jj, adjective).
lexmap(jj+jr, adjective).
lexmap(jjr, adjective).
lexmap(jjs,adjective).

% Determiners: Number features (might as well use no features)
lexmap(abl, determiner(N)) :- num(N).
lexmap(abn, determiner(N)) :- num(N).
lexmap(abx, determiner(N)) :- num(N).
lexmap(ap, determiner(N))  :- num(N).
lexmap(ap-s, determiner(N)) :- num(N).
lexmap(ap+ap, determiner(N)) :- num(N).
lexmap(at,determiner(N)) :- num(N).
lexmap(dt,determiner(N)) :- num(N).

% Wh-determiners
lexmap(wdt,wh_determiner).
lexmap(wp-s,wh_determiner).
lexmap(wpo,wh_determiner).
lexmap(wps,wh_determiner).

% Nouns: countable, num, gender
lexmap(nn, noun(countable,sing,uninflicted)).
lexmap(nn-s, noun(countable,sing,genitive)).
lexmap(nn+nn, noun(countable,sing,uninflicted)).
lexmap(nns, noun(countable,plur,uninflicted)).
lexmap(nns-s, noun(countable,plur,genitive)).

% adverbial nouns
lexmap(nr, noun(countable,sing,uninflicted)).
lexmap(nr-s, noun(countable,sing,genitive)).
lexmap(nrs, noun(countable,plur,uninflicted)).

% proper nouns
lexmap(np, propernoun(countable,sing,uninflicted)).
lexmap(np-s, propernoun(countable,sing,genitive)).
lexmap(nps, propernoun(countable,plur,uninflicted)).
lexmap(nps-s, propernoun(countable,plur,genitive)).

% ordinals, cardinals and quantifiers
lexmap(od,ordinal(Number)) :- num(Number).
lexmap(cd,cardinal(Number)) :- num(Number).
lexmap(ap,quantifier(Number)) :- num(Number).

% Conjunctions
lexmap(cc, conjunction). % subordinating
lexmap(cs, conjunction). % coordinating

% Prepositions
lexmap(in, preposition).
lexmap(in+in, preposition).

% Pronouns
% Brown is not so detailed, so we also use our own definitions
lexmap(pn, pronoun(Number,Person,Case,Gender)) :-
	num(Number),
	person(Person),
	subj_obj(Case),
	gender(Gender).
lexmap(pn-s,  pronoun(Number,Person,possesive-determiner,Gender)) :-
	num(Number),
	person(Person),
	gender(Gender).
lexmap(pp-ss, pronun(Number,Person,possesive-nominal,Gender)) :-
	num(Number),
	person(Person),
	gender(Gender).
lexmap(ppl,pronoun(sing,Person,reflexive,Gender)) :-
	person(Person),
	gender(Gender).
lexmap(ppl,pronoun(plural,Person,reflexive,Gender)) :-
	person(Person),
	gender(Gender).
lexmap(ppo,pronoun(Number,Person,objective,Gender)) :-
	num(Number),
	person(Person),
	gender(Gender).
lexmap(pps,pronoun(sing,third,subjective,Gender)) :-
	gender(Gender).
lexmap(ppss,pronoun(sing,third,subjective,Gender)) :-
	gender(Gender).	

% Relative pronouns
lexmap(wp-s,relative_pronoun(genitive,Gender)) :-
	gender(Gender).
lexmap(wpo,relative_pronoun(accusative,Gender)) :-
	gender(Gender).
lexmap(wps,relative_pronoun(nominal,Gender)) :-
	gender(Gender).

% Determiners,
lexmap(abl,determiner(N)) :- num(N).
lexmap(abn,determiner(N)) :- num(N).
lexmap(abx,determiner(N)) :- num(N).
lexmap(ap,determiner(N)) :- num(N).
lexmap(ap+ap,determiner(N)) :- num(N).
lexmap(at,determiner(N)) :- num(N).
lexmap(dt,determiner(sing)).
lexmap(dti,determiner(N)) :- num(N).
lexmap(dts,determiner(plur)).
lexmap(dtx,determiner(N)) :- num(N).

% Adverbs
lexmap(rb, adverb(normal)). % normal adverb ending in -ly
lexmap(rn, adverb(nominal)). % there,here,then
lexmap(ex, adverb(nominal)). % existential there
lexmap(rp, adverb(participle)). % up,in,out,down
lexmap(rbt, adverb(superlative)). % highest, fastest
lexmap(rbr, adverb(comparative)). %  higher, faster
lexmap(wrb, adverb(normal)). % wh-adverb :  however, when, whereby, why
lexmap(ql, adverb(qualifier)).
lexmap(qlp, adverb(qualifier)).

% qualifier
lexmap(ql, qualifier(pre)).
lexmap(qlp, qualifer(post)).

% punctuation
lexmap(comma, comma).
lexmap(start_paran,start_paran).
lexmap(end_paran,end_paran).
lexmap(dot, dot).
lexmap(colon,colon).
