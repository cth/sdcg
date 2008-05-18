% Small prolog lexicon:
expand_mode(number(-,+)).
expand_mode(gender(-,+)).
expand_mode(wordlist(-,+)).

word(he,sg,masc).
word(she,sg,fem).

number(Word,Number) :- word(Word,Number,_).
gender(Word,Gender) :- word(Word,_,Gender).
wordlist(X,[X]).

% Start rule
sdcg ==> word(_,_).
% Grammar rule:
word(@number(Word,N), @gender(Word,G)) ==> @wordlist(Word, WordList).
