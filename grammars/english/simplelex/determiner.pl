% 
% Determiners
% Determiners have similar properties to pronouns. Some pronouns can also be be used as determiners (eg. possessive).
% Article: The use of "a" and "an" depends on the following noun. If it begins with an vowel sound, then it's "an" otherwise it's "a".
% I contemplating modelling this dynamically, by testing the the noun.. 
%
% Features:
% Number: sing/plur
% Gender: masc/fem/neut

det(sing,@gender(G),a) ==> [a].
det(sing,@gender(G),an) ==> [an].
det(sing,@gender(G),the) ==> [the].
det(plur,@gender(G),the) ==> [the].

det(sing,@gender(G),this) ==> [this].
det(sing,@gender(G),that) ==> [that].
det(plur,@gender(G),these) ==> [these].
det(plur,@gender(G),those) ==> [those].

det(@number(N),@gender(G),some) ==> [some].
det(@number(N),@gender(G),every) ==> [every].
