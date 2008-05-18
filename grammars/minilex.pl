% Contains just a few of each word category. Mainly for testing.

%% Nouns %%
lexitem(noun,sg,dog).
lexitem(noun,pl,dogs).
lexitem(noun,sg,boy).
lexitem(noun,pl,boys).
lexitem(noun,sg,idea).
lexitem(noun,pl,ideas).

%% Verbs %%
lexitem(verb,sg,runs).
lexitem(verb,pl,run).
lexitem(verb,sg,plays).
lexitem(verb,pl,play).

%% Prepositions %%
lexitem(prep,in).
lexitem(prep,on).

%% Adjectives %%
lexitem(adj,N,green) :- sgpl(N).
lexitem(adj,N,colorless) :- sgpl(N).

%% Adverbs %%
lexitem(adverb,N,furiously) :- sgpl(N).
lexitem(adverb,N,well) :- sgpl(N).

sgpl(sg).
sgpl(pl).

%% Utitilies

word(W) :- lexitem(_,_,W).
words(Ws) :- setof(X,lexitem(_,_,X),Ws).