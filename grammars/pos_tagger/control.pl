%pos_tag_sentence(Sentence, PosTagged) :-
%learn_pos_tagger :-
%	acquire_samples(Samples),
	
% Is used in expansion by the grammar
% All words, where each is enclosed in brackets
word_l([W]) :-
	brown_word(W).

% Add a none tags, to signal that the preceeding tag has no tags.
brown_tag(none).
brown_tag(sometag).

brown_word('alpha').
brown_word('beta').
brown_word('gamma').

