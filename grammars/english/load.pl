% Load compiler
:- cl('../../sdcg.pl').
:- sdcg_set_option(debug).
:- sdcg_set_option(maxdepth,4).
%:- sdcg_set_option(parsetree).
:- sdcg_set_option(use_foc_cheat).
%:- sdcg_set_option(check_generated_program).
	
% Load the lexicon (simple)
:- include_rel('grammars/english/simplelex/helpers.pl').
:- include_rel('grammars/english/simplelex/lexicon_static.pl').

:- resolve_path('grammars/english/lexicon_rules.pl',LexRules), sdcg_parse(LexRules).

% Parse the english grammar:
:- resolve_path('grammars/english/english.pl',GrammarFile), sdcg_parse(GrammarFile).

% Compile the grammar to a PRISM program
:- done_parsing.