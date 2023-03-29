% file: main.pl
% origin author : J. Kuhn
% origin date: June 2021
% purpose: top executable for MG Generator
%:- ['generator'].
:- ['select'].
:- ['fillIn'].
%:- ['select0'].
:- ['spellOut'].
:- ['lambdaWorkspace'].
:- ['helpers/painter'].
:- ['helpers/stuff'].
%:- ['grammars/maus'].


makeFromExprWithFilling(Expr,Sentance,Lambda,Tree):-
  lambdaSelectFkt(Expr,List),
  epsilonSelectFkt(List,Filled),
  generate(Filled,Tree),
  spellCheck(Expr,Tree,Sentance,Lambda),
  tree_painter(Tree).


% example:
%
% makeFromExprWithFilling(find(hungry(mouse),loc(cheese,field(bFa(b,t10plus(6,4))))),Sentance,Lambda,Tree).