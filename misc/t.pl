:- cl('../compiler/sdcg.pl').

test :-
	% setup 
	assert(number(he,sg)),
	assert(expand_mode(number(-,+))),
	% test:
	Expander =.. [ number, X, Y ],
	resolve_expand_mode(Expander, ModeList),
	%	write(ModeList),nl,
	ModeList == [-,+], % correctness assertion
	% cleanup
	retract(number(he,sg)),
	retract(expand_mode(number(-,+))).

test2 :-
        % setup 
        assert(word(he,sg,masc)),
        assert(expand_mode(word(-,+,+))),
        % test:
	Args = [ a,b,c ],
        Expander =.. [ word | Args ],
        resolve_expand_mode(Expander, ModeList),
	arg_expand_list(Args, ModeList,NewArgList),
	write('New arg list; '), write(NewArgList),nl,
        % cleanup
        retract(word(he,sg,masc)),
        retract(expand_mode(word(-,+,+))).
