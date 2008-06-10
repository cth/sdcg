% Load compiler
:- cl('../../config.pl').
:- require('compiler/sdcg.pl').
:- sdcg_set_option(debug,yes).
:- sdcg_set_option(maxdepth,2).

% Load the lexicon (simple)
:- require('grammars/english/simplelex/simplelex.pl').
:- simplelex_load.

% Parse the english grammar:
:- resolve_path('grammars/english/minienglish.pl',GrammarFile), sdcg_parse(GrammarFile).

% Compile the grammar to a PRISM program
:- sdcg_compile.