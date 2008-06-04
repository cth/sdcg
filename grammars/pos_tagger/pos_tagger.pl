%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is a simple POS tagger expressed using SDCG
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Load compiler and brown corpus
:- cl('../../config.pl').
:- require('compiler/sdcg.pl').
:- require('corpora/brown/brown_parts.pl').
:- require('corpora/brown/brown_helper.pl').

% Load the POS control program
:- require('grammars/pos_tagger/control.pl').

% Load the grammar
:- require('grammars/pos_tagger/grammar.pl').

% Test the tagger and report on performance.
:- sdcg_compile.