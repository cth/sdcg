load :-
	cl('../../config.pl'),
	[control],
	require('compiler/sdcg.pl'),
	sdcg_set_option(prism_invoker,prism),
	sdcg('grammar.pl').