% Very simple grammar which demonstrates the use the extended DCG syntax
% used for testing rule expansion

% A normal prolog predicate which is referenced from the grammar
digit([X]) :- member(X,[0,1,2,3,4,5,6,7,8,9]).

% The grammar rules
sdcg ==> set. % start rule
number ==> +(digit), ?(comma), +(digit).
number ==> +(digit).
set ==> *(number).
digit ==> @digit(X). % expands by calling digit/1
comma ==> [ ',' ].
