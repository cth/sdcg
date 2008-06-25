%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is the main file of the SDCG compiler library.
% You need to _compile_ this file to use the library.
% Also, to setup paths on your system, you will either have to
% edit the directive below, or set it manually from your own 
% code:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This is the path to directory where the SDCG compiler is located:
% Edit this to reflect the path of your installation.
sdcg_directory('/Users/cth/Documents/code/sdcg').

% Load the compiler library
:- 	sdcg_directory(Dir), 
	atom_concat(Dir, '/util/util.pl',Utilities),
	cl(Utilities),
	atom_concat(Dir, '/compiler/sdcg.pl', Compiler),
	cl(Compiler).
