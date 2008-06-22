:- cl('../config.pl').
:- require('compiler/sdcg.pl').

:- sdcg_set_option(debug,true).
:- sdcg_set_option(maxdepth,3).
:- sdcg('../grammars/test/new_expand_test.pl').