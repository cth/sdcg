%:-['../util/parsetree.pl'].

example1(
[s,
	[np,
		[det, 
		noun]],
	[vp,
		[verb]]
]
).

parse_tree_rules([],[],[]).
parse_tree_rules([Root|Children], Rule, [Rule|RulesRest]) :-
	children_rules(Children,ChildrenRules, RulesRest),
	heads(ChildrenRules,ChildrenHeads),
	RuleList = [ ==>, Root, ChildrenHeads],
	flatten(RuleList,FlatRuleList),
	Rule =.. FlatRuleList.
parse_tree_rules(Leaf,Rule)

children_rules([Child|Siblings],[ChildRule|SiblingRules], [RecChildRules|RecSiblingRules]) :-
	parse_tree_rules(Child,ChildRule,RecChildRules),
	children_rules(Siblings,SiblingRules,RecSiblingRules).

heads([],[]).
heads([Rule|RulesRest],[Head|HeadsRest]) :-
	head(Rule,Head),
	heads(RulesRest,HeadsRest).
	
head([==>,Head|_],Head).



test :-
	example1(ParseTree),
	%write('Parse tree is: '), write(ParseTree), nl,
	parse_tree_rules(ParseTree, _, Rules),
	%write('Parse tree rules are: '),nl,
	write(Rules).