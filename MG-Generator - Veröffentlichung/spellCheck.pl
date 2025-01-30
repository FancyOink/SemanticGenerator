:- module(spellCheck,[spellCheck/4]).
% file: spellCheck.pl
% origin author : J. Kuhn
% origin date: May 2021
% purpose: checks a derivation tree according to provided/ predefined rules

spellCheck(Expr,li(Sentance,_,Expr),Sentance,Expr).
spellCheck(Expr,tree([(Sentance,_,Expr)],_,_,_),Sentance,Expr).
