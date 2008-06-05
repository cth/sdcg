% Relative pronouns
% Case: nominative/accusative/genitive
% Gender: masc/fem/neut

% Accusative: (brown tag WPO)
relative_pronoun(accusative,@human(G),'whom') ==> [ 'whom' ].
relative_pronoun(accusative,@human(G),'who') ==> [ 'who' ].
relative_pronoun(accusative,@non_human(G),'that') ==> [ 'that' ].

% Nominative:
relative_pronoun(nominative,@human(G),'who') ==> ['who'].
relative_pronoun(nominative,@human(G),'whoever') ==> ['whoever'].
relative_pronoun(nominative,@human(G),'whosoever') ==> ['whosoever'].
relative_pronoun(nominative,@non_human(G),'that') ==> ['that'].
relative_pronoun(nominative,@non_human(G),'what') ==> ['what'].
relative_pronoun(nominative,@non_human(G),'whatever') ==> ['whatever'].
relative_pronoun(nominative,@non_human(G),'whatsoever') ==> ['whatsoever'].

% Genitive:
relative_pronoun(genitive,@human(G),'whose') ==> ['whose'].
relative_pronoun(genitive,@human(G),'whosever') ==> ['whosever'].