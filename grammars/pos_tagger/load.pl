% Load compiler
:- cl('../../sdcg.pl').
%:- sdcg_set_option(debug).

% Since the part of speech tagger recursively calls the next rule
% for each consumed word, the maxdepth option is used to express
% the maximal sentence length.
% (except, the regex in the grammar creates an extra rule, so we need to double this)
:- sdcg_set_option(maxdepth,6).

:- sdcg_set_option(use_foc_cheat).
%:- sdcg_set_option(check_generated_program).
	
% Parse the english grammar:
:- resolve_path('grammars/pos_tagger/grammar.pl',GrammarFile), sdcg_parse(GrammarFile).

% Compile the grammar to a PRISM program
:- done_parsing.

train :-
	learn([
		start([det,noun,modalverb,verb],[the,can,will,rust],[]),
		start([det,noun],[the,rust],[]),
		start([modalverb,noun,verb],[will,rust,rust],[]),
		start([noun,modalverb,verb],[will,can,rust],[])
	]),
	show_sw.