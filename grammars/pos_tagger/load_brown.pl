% This 

% Load compiler
:- cl('../../sdcg.pl').
%:- sdcg_set_option(debug).

% Since the part of speech tagger recursively calls the next rule
% for each consumed word, the maxdepth option is used to express
% the maximal sentence length.
% (except, the regex in the grammar creates an extra rule, so we need to double this)
:- sdcg_set_option(maxdepth,22).

%:- sdcg_set_option(use_foc_cheat).
:- sdcg_set_option(use_append).
%:- sdcg_set_option(check_generated_program).

:- require('corpora/brown/wordlist.pl').
:- require('corpora/brown/tagmap.pl').

%tag(X) :-
%	tagmap(_,X).
	
tag(a).

% Parse the english grammar:
:- resolve_path('grammars/pos_tagger/grammar.pl',GrammarFile), sdcg_parse(GrammarFile).

% Compile the grammar to a PRISM program
:- done_parsing.