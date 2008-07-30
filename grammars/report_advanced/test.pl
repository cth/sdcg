:- op(1200, xfx, @=>).

lex(determiner,the).
lex(determiner,a).

headword(foo).
headword(bar).

expander(E) :-
        lex(determiner, A),
        headword(B),
        C=..['|',det(_,A),B],
        D=[A],
        E=(C@=>D).
