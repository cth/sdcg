:- cl('../../sdcg.pl').
:- sdcg_set_option(debug).
:- sdcg_set_option(parsetree).
%:- sdcg_set_option(maxdepth,10).
:- sdcg_set_option(use_foc_cheat).
:- sdcg('../../grammars/report_sample/cond_sample.pl').

% Some training data to
train :-
	learn([	start([time,flies],[]),	start([flies,fly],[]),	start([time,crawls],[]), start([time,flies,fly],[])	]),
	show_sw.
	
train_with_parsetree :-
	learn([	start([time,flies],[],_),	start([flies,fly],[],_),	start([time,crawls],[],_), start([time,flies,fly],[],_)	]),
	show_sw.