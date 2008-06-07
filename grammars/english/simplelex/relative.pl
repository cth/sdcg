%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Relative pronouns
% Case: nominative/accusative/genitive
% Gender: masc/fem/neut
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Accusative: (brown tag WPO)
relative_pronoun(accusative,@human(_G),@enum([who,whom],Stem)) ==> [Stem].
relative_pronoun(accusative,neut,@enum([that,what,which],Stem)) ==> [Stem].

% Nominative:
relative_pronoun(nominative,@human(_G),@enum([who,whoever, whosoever],Stem)) ==> [Stem].
relative_pronoun(nominative,neut,@enum([that,what,whatever,whatsoever],Stem)) ==> [Stem].

% Genitive:
relative_pronoun(genitive,@human(_G),@enum([whose,whosever],Stem)) ==> [Stem].
