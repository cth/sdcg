:- cl('../../sdcg.pl').
:- cl('lexicon.pl').
%:- sdcg_set_option(debug).
%:- sdcg_set_option(parsetree).
:- sdcg_set_option(maxdepth,10).
:- sdcg_set_option(use_foc_cheat).
:- sdcg('grammar.pl').