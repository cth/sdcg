% FIXME: I Think these are defined elsewhere..
wh ==> [which].
wh ==> [what].
wh ==> [who].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cardinals
cardinal(sing,one).
cardinal(plur,two).
cardinal(plur,three).
cardinal(plur,four).
cardinal(plur,five).
cardinal(plur,six).
cardinal(plur,seven).
cardinal(plur,eight).
cardinal(plur,nine).
cardinal(plue,ten).
cardinal(@cardinal(_Number,Stem)) ==> [ Stem ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ordinals

% Ordinals that may occur with any count
ordinal(@number(_Num),@enum([first,next,second,last,other],Stem)) ==> [Stem].

% Ordinals that may only occur with plural count nouns
ordinal(plur, @enum(many,Stem)) ==> [Stem]. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Quantifier
quantifier(@number(_Number),@enum([first class,nonstop,longest,earliest],Stem)) ==> [Stem].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Conjunctions
@conjunction(@enum([and,or,but],Stem)) ==> [Stem].