% file: select.pl
% origin author : J. Kuhn
% origin date: May 2021
% purpose: selects LI from a Lexicon according to predicate logical expression

:- op(500, xfy, ::). % infix predicate for lexical items
:- op(500, fx, =). % for selection features

:- ['helpers/stuff'].

%:- ['grammars/zahlenLambdaEinfachV1'].
:- ['grammars/maus'].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% selectFkt(+Prädikatenlogischer_Ausdruck,-[Li]).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lambdaSelectFkt(Input,Output):-
    lambdaFitting(Input,[],Output),write("List of Li found: "),writeln(Output).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% lambdaFitting(+Expr_to_be_matched,+[Expr],-[Li]).
%
% This functions checks the first expression and if it can be matched to a Lambda-expression in the lexicon adds the corresponding Li to the output-List
% potential further expressions are added to [Expr]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lambdaFitting([],[],[]).
lambdaFitting(In,Expr,Out):-
    In =.. InputFkt, writeln(In),checkLambda(InputFkt,PotExprList,Li),append(Expr,PotExprList,NewExprList), (NewExprList = [NewIn|Rest]; NewIn = [],Rest = []), lambdaFitting(NewIn,Rest,DeeperOut),
      append(DeeperOut,[Li],Out);    % Version, mit first in, last out
    %  Out = [Li|DeeperOut];           % Version mit first in first out, kann zu vertauschung führen
    Expr = [NewIn|Rest],lambdaFitting(NewIn,Rest,Out). % Ist der Teil hier legal? Unter welchen Umständen lass ich einen Ausdruck zu, der nicht mit einem Lambda aus dem Lexikon gepaart werden kann?
lambdaFitting(_,_,[]).
% und(eins,zwei) =.. [Name|Rest],length(Rest,Arg),functor(C,Name,Arg),W::(Fs,L>> _ = C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkLambda(+Expression, -[PotExprList], -Li)
%
% NB: erweitere die Funktion später um eine Liste an möglichen Ausdrücken rauszufinden und weiterzugeben
% This function tries to unifiy the input expression with one of the lexical items, according to the lambda expression.
% It furthermore identifies potential expressions to be checked inside the expression under investigation.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


checkLambda([Name],PotExprList,Li) :- writeln("Constant!"),W::(Fs,Name), Li = li(W,Fs,Name),PotExprList=[], writeln(Li).  % Expression is a constant
checkLambda([Name|Input],PotExprList,Li):- writeln("Funktion!"),                                                          % Expression is a function
    length(Input,Length),length(Rest,Length),
    FktLambda =..[Name|Rest],W::(Fs,L >> (Out = FktLambda)),Li = li(W,Fs,L >> (Out = FktLambda)),writeln(Li),
    makePotList(Input,Rest,PotExpr),
    sortFromFkt(L,Rest,PotExpr,PotExprRe),
    reverse(PotExprRe,PotExprList),
    write("Potential Expressions: "),writeln(PotExprList).
checkLambda(_,_,[]):- writeln("Catch all.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% makePotList(+[InExpr],+[LexExpr],-[Expr])
%
% NB: wie bei oberer Funktion auf Fall meherer möglicher Ausdrücke aus dem Lexikon erweitern
% This function checks if the input expression are already filled in by the lexical expression and if so, removes them from the list
% e.g. ['weis nix'] :: ([=d,v],[X,Out]>> (Out = weis(X,nix))), in this case "nix" is already filled in and neend not be considered by the rest of the Lexical search
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

makePotList([],[],[]).
makePotList([A|RestA],[B|RestB],PotExpr):-
  nonvar(A), var(B),
    makePotList(RestA,RestB,DeeperPot),append([A],DeeperPot,PotExpr);
  nonvar(A),nonvar(B),
    A =..[NameA|InputA], B =.. [NameB|InputB],
    NameA == NameB,                                                                                                         % NB: Fehlerbehandlung einfügen, wenn es hier gebricht
      makePotList(InputA,InputB,InputPot),
      makePotList(RestA,RestB,DeeperPot),
      append(InputPot,DeeperPot,PotExpr);
  makePotList(RestA,RestB,PotExpr).                                                                                         % NB: catch all, sollte mit der Fehlerbehandlung oben später abgestimmt werden



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% sortFromFkt(+[Lambda_list],+[Lambda_function_input],+[Pot_Expressions],-[sorted_Pot_Expressions])
%
% This function sorts the List of Potential Expressions accroding to their occurences in the Lambda-Expression of the Li
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% findall(W::(Fs,epsilon),W::(Fs,epsilon),L).
sortFromFkt(_,_,[],[]).
sortFromFkt([_],_,Sort,Sort).
sortFromFkt([HeadL|LList],TList,ToSort,OutList):- matchVariable(HeadL,TList,ToSort,MatchVar,RestTList,RestToSort), sortFromFkt(LList,RestTList,RestToSort,DeeperList),OutList = [MatchVar|DeeperList].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% matchVariable(+Var_to_Match, +[List_to_match], -[List_to_match_after], -found_match, -[List_without_match], -[List_for_other_matches])
%
% This function tries to match the first variable with the head of the list and than sorts that list with the variable at the front
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matchVariable(_,[],_,_,_,_):-false.
matchVariable(HeadL,[THead|TRest],[HeadSort|ToSort],Match,RestTList,RestToSort):-
  var(THead), HeadL == THead,
    Match = HeadSort, RestTList = TRest, RestToSort = ToSort;
  nonvar(THead),
    THead =.. [Name|FktRest], matchVariable(HeadL,FktRest,[HeadSort|ToSort],Match,DeeperT,DeeperSort),NewHead=..[Name,DeeperT],RestTList = NewHead,RestToSort=DeeperSort;
  (nonvar(THead),THead =.. [_|Rest],length(Rest,Leng),dropSort(Leng,[HeadSort|ToSort],RestSort);RestSort = ToSort),
    matchVariable(HeadL,TRest,RestSort,Match,DeeperT,DeeperSort), RestTList = [THead|DeeperT], RestToSort = [HeadSort|DeeperSort].
