% Load compiler
:- cl('../../sdcg.pl').
:- sdcg_set_option(debug).
:- sdcg_set_option(maxdepth,16).
%:- sdcg_set_option(parsetree).
%:- sdcg_set_option(use_foc_cheat).
:- sdcg_set_option(use_append).
%:- sdcg_set_option(check_generated_program).
:- sdcg_set_option(postload_verification_check).

% Set this to yes if you want to account for the
% probability of the preterminal rules
%match_inline(no).

% Load the lexicon
:- include_rel('grammars/english/brownlex/tagwords.pl').
:- include_rel('grammars/english/brownlex/lexmap.pl').
:- include_rel('grammars/english/brownlex/lexhelper.pl').

% Parse the lexicon grammar rules
:- resolve_path('grammars/english/brownlex/lexrules.pl',LexRules), sdcg_parse(LexRules).

% Parse the english grammar:
:- resolve_path('grammars/english/english.pl',GrammarFile), sdcg_parse(GrammarFile).

% Compile the grammar to a PRISM program
:- done_parsing.