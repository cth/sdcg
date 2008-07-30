test_set_unset_option :-
	sdcg_set_option(parsetree,true),
	sdcg_option(parsetree,true),
	sdcg_unset_option(parsetree),
	sdcg_option(parsetree,false).
