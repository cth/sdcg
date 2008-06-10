% This is the grammar. Load regex.pl to use it.

sdcg ==> s. % Start rule
s ==> *(a). % Should generate all three types of regex rules
a ==> [test].