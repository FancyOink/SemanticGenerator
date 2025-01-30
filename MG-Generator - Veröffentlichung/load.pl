% file: load.pl
% origin author : J. Kuhn
% origin date: November 2024
% purpose: load file for the semantic generator

:- set_prolog_flag(encoding,utf8).
:- use_module(library(ordsets)).
:- use_module(library(ugraphs)).
:- op(500, xfy, ::). % infix predicate for lexical items
:- op(500, fx, =). % for selection features
%:- op(500, fx, #). % for category features, if the need arises to redefine the sign of Category features

:- ['lambdaSelect'].
:- ['ypsilonSelect'].
:- ['spellCheck'].
:- ['lambdaWorkspace'].
:- ['helpers/painter'].
:- ['helpers/stuff'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Used Lexikon for the generator
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ['grammars/German'].
