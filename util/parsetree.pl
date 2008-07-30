%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rules for converting between parse trees and sdcg rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% We assume that parse trees are given as Prolog lists.
% FIXME: Not done

parse_tree_rules([],[],[]).
parse_tree_rules([Root, Children], Rule, [Rule|RulesRest]) :-
	children_rules(Children,ChildrenRules, RulesRest),
	heads(ChildrenRules,ChildrenHeads),
	RuleList = [ ==>, Root, ChildrenHeads],
	flatten(RuleList,FlatRuleList),
	Rule =.. FlatRuleList.

children_list_rules([Child|Siblings],[ChildRule|SiblingRules], [RecChildRules|RecSiblingRules]) :-
	parse_tree_rules(Child,ChildRule,RecChildRules),
	children_list_rules(Siblings,SiblingRules,RecSiblingRules).

heads([],[]).
heads([Rule|RulesRest],[Head|HeadsRest]) :-
	head(Rule,Head),
	heads(RulesRest,HeadsRest).
	
head([==>,Head|_],Head).