:- cl('../config.pl').
:- require('compiler/sdcg.pl').

:- sdcg_set_option(debug,true).
%:- sdcg_set_option(maxdepth,3).
%:- sdcg('../grammars/test/new_expand_test.pl').


v(a).
v(b).
v(c).

conditioning_mode(s(+)).

start ==> s(a).
s(X) | @v(X) ==> s(X).
s(X) | @v(X) ==> [X].