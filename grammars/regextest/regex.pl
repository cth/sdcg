% Loads regex grammar
:- cl('../../config.pl').

:- require('compiler/sdcg.pl').

:- sdcg_set_option(debug,yes).
:- sdcg_set_option(prism_file,'regex.psm').
:- sdcg('grammar.pl').
