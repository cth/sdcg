% Assumes that SDCG compiler is loaded.
lexicon_directory("grammars/english/simplelex/").
lexicon_files([helpers,noun,determiner,preposition,punctuation,relative,verb,other]). 

only_some_lexicon_files([helpers,noun,verb]). %,other,preposition,punctuation,relative,verb]).
%lexicon_files([helpers,verb]).


load_lexicon_files([]).
load_lexicon_files([File|Files]) :-
	lexicon_directory(LexDir),
	atom_codes(File,FileStr),
	append(FileStr,".pl",FileStrSuffixed),
	append(LexDir,FileStrSuffixed,LexFileStr),
	atom_codes(LexFile,LexFileStr),
	resolve_path(LexFile,AbsLexFile),
	write(AbsLexFile),nl,
	sdcg_parse(AbsLexFile),
	load_lexicon_files(Files).

simplelex_load :-
	lexicon_files(L),
	load_lexicon_files(L).
	
simplelex_load_mini :-
	only_some_lexicon_files(L),
	load_lexicon_files(L).