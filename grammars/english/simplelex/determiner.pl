% 
% Determiners
% Determiners have similar properties to pronouns. Some pronouns can also be be used as determiners (eg. possessive).
% Article: The use of "a" and "an" depends on the following noun. If it begins with an vowel sound, then it's "an" otherwise it's "a".
% I contemplating modelling this dynamically, by testing the the noun.. 
%
% Features:
% Number: sing/plur
% Gender: masc/fem/neut

det(sing,@gender(_G),@enum([a,an,this,that],Stem)) ==> [Stem].
det(plur,@gender(_G),@enum([the,these,those],Stem)) ==> [Stem].
det(@num(_N),@gender(_G),@enum([the,some,every],Stem)) ==> [Stem].
wh_determiner(@enum([which,what,whatever,whichever],Stem)) ==> [Stem].