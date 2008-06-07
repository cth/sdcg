%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Edit this file to set up paths etc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Absolute path where the sdcg code is located
basedir('/Users/christianhave/code/sdcg/sdcg/').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% You should not have to edit this section
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resolve_path(Relative,Absolute) :-
	basedir(Basedir),
	atom_codes(Basedir,BasedirChars),
	atom_codes(Relative,RelativeChars),
	append(BasedirChars,RelativeChars,AbsoluteChars),
	atom_codes(Absolute,AbsoluteChars).
	
% require compiles and loads a file within the distribution based
% on a path relative to the base directory
require(File) :-
	resolve_path(File,AFile),
	cl(AFile).
	
include_rel(File):-
	resolve_path(File,AFile),
	[AFile].