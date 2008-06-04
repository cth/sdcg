wh ==> [which].
wh ==> [what].
wh ==> [who].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cardinals
cardinal(sing,one).
cardinal(plur,two).p
cardinal(plur,three).
cardinal(plur,four].
cardinal(plur,five).
cardinal(plur,six).
cardinal(plur,seven).
cardinal(plur,eight).
cardinal(plur,nine).
cardinal(plue,ten).
cardinal(@cardinal(Number,Stem)) ==> [ Stem ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ordinals
ordinal(@number(X), first) ==> [ first ].
ordinal(@number(X), next) ==> [next].
ordinal(@number(X), second) ==> [second].
ordinal(@number(X), last) ==> [last].
ordinal(@numver(X), other) ==> [other].
ordinal(plur, many) ==> [many]. % May only occur with plural count nouns

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Quantifier
quantifier(@number(X),first_class) ==> [first,class].
quantifier(@number(X),nonstop) ==> [nonstop].
quantifier(@number(X),longest) ==> [longest].
quantifier(@number(X),earliest) ==> [earliest].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Conjunctions
conjunction(and) ==> [and].
conjunction(or) ==> [or].
conjunction(but) ==> [but].