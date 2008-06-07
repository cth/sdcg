% Load compiler
:- cl('../../config.pl').
:- require('compiler/sdcg.pl').
:- sdcg_set_option(debug,yes).

% Load the test grammar
:- resolve_path('grammars/test/test_quoted_params.pl',File),sdcg_parse(File).
:- sdcg_compile.