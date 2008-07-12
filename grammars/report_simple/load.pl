sdcg_directory('../..').
:- cl('../../sdcg.pl').
:- sdcg_set_option(debug).
:- sdcg_set_option(parsetree).
%:- sdcg_set_option(maxdepth,10).
:- sdcg_set_option(use_foc_cheat).
:- sdcg('grammar.pl').

% Some training data to 

train :-
	learn([	start([time,flies],[]),	start([flies,fly],[]),	start([time,crawls],[]), start([time,flies,fly],[])	]),
	show_sw.
	
train_with_parsetree :-
	learn([	start([time,flies],[],_),	start([flies,fly],[],_),	start([time,crawls],[],_), start([time,flies,fly],[],_)	]),
	show_sw.
/*	
testit :-
	prob(start([time,flies],[]),P1),
	write(time_flies-), write(P1), nl,
	prob(start([time,time],[]),P2),
	write(time_flies-), write(P2), nl.
*/