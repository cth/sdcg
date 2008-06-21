% Load compiler
:- cl('../../config.pl').
:- require('compiler/sdcg.pl').
:- sdcg_set_option(debug,yes).
:- sdcg_set_option(maxdepth,10).

% Parse the english grammar:
:- resolve_path('grammars/english/simplelex/lexicon_static.pl', StaticLex), sdcg_parse(StaticLex).
:- resolve_path('grammars/english/lexicon_rules.pl',LexRules), sdcg_parse(LexRules).
%:- resolve_path('grammars/english/minienglish.pl',GrammarFile), sdcg_parse(GrammarFile).

% Compile the grammar to a PRISM program
%:- sdcg_compile.