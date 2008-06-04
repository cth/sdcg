%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Edit this file to set up paths etc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Absolute path where the sdcg code is located
basedir('/Users/christianhave/code/sdcg/').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% You should not have to edit this section
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% require compiles and loads a file within the distribution based
% on a path relative to the base directory
require(File) :-
	basedir(Basedir),
	atom_codes(Basedir,BasedirChars),
	atom_codes(File,FileChars),
	append(BasedirChars,FileChars,LoadFileChars),
	atom_codes(LoadFile,LoadFileChars),
	cl(LoadFile).
	
include(File):-
	basedir(Basedir),
	atom_codes(Basedir,BasedirChars),
	atom_codes(File,FileChars),
	append(BasedirChars,FileChars,LoadFileChars),
	atom_codes(LoadFile,LoadFileChars),
	[LoadFile].
