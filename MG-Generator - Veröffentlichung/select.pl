:- module(select,[lambdaSelectFkt/2]).
% file: select.pl
% origin author : J. Kuhn
% origin date: May 2021
% purpose: selects LI from a Lexicon according to predicate logical expression

%sDebugMode.
sDebugMode :- false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% selectFkt(+Prädikatenlogischer_Ausdruck,-[Li]).
%NB:  Gruppe der Outgoing Li anpassen, an mögliche Umordnung durch andere Abstraktionsreihenfolge
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lambdaSelectFkt(Input,Output):-
    lambdaFitting((Input,1,[0]),[],Output),(sDebugMode -> write("List of Li found: "),writeln(Output);true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% lambdaFitting(+Expr_to_be_matched,+[Expr],-[Li]).
% Expr_ti_be_matched consists of an Expression, an integer for the depth of the expression and a list of the group it belongs to
% This functions checks the first expression and if it can be matched to a Lambda-expression in the lexicon adds the corresponding Li to the output-List
% potential further expressions are added to [Expr]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lambdaFitting([],[],[]).
lambdaFitting((In,T,G),Expr,Out):-
    In =.. InputFkt, (sDebugMode -> writeln(In);true),checkLambda(InputFkt,T,G,PotExprList,Li),append(Expr,PotExprList,NewExprList), (NewExprList = [NewIn|Rest]; NewIn = [],Rest = []), lambdaFitting(NewIn,Rest,DeeperOut),
      append(DeeperOut,[Li],Out);    % Version, mit first in, last out
    %  Out = [Li|DeeperOut];           % Version mit first in first out, kann zu vertauschung führen
    Expr = [NewIn|Rest],lambdaFitting(NewIn,Rest,Out). % Ist der Teil hier legal? Unter welchen Umständen lass ich einen Ausdruck zu, der nicht mit einem Lambda aus dem Lexikon gepaart werden kann?
lambdaFitting(_,_,[]).
% und(eins,zwei) =.. [Name|Rest],length(Rest,Arg),functor(C,Name,Arg),W::(Fs,L>> _ = C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkLambda(+Expression,+Deepth,+[Group], -[PotExprList], -Li)
%
% NB: erweitere die Funktion später um eine Liste an möglichen Ausdrücken rauszufinden und weiterzugeben (Stelligkeit/Valenz)
% This function tries to unifiy the input expression with one of the lexical items, according to the lambda expression.
% It furthermore identifies potential expressions to be checked inside the expression under investigation.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


checkLambda([Name],T,G,PotExprList,Li) :- (sDebugMode -> writeln("Constant!");true),W::(Fs,Name), Li = lambdaLi(W,Fs,Name,T,G),PotExprList=[], (sDebugMode -> writeln(Li);true).  % Expression is a constant
checkLambda([Name|Input],T,G,PotExprList,Li):- (sDebugMode -> writeln("Funktion!");true),                                                          % Expression is a function
    length(Input,Length),length(Rest,Length),
    FktLambda =..[Name|Rest],W::(Fs,L >> (Out = FktLambda)),Li = lambdaLi(W,Fs,L >> (Out = FktLambda),T,G),(sDebugMode -> writeln(Li);true),T2 is T + 1,
    makePotList(Input,T2,(1,G),_,Rest,PotExpr), % geht Tiefer in die Semantik
    sortFromFkt(L,Rest,PotExpr,PotExprRe), % sort by occurence in the Lambda-function
    reverse(PotExprRe,PotExprList), % reverse, because FILO
    (sDebugMode -> write("Potential Expressions: "),writeln(PotExprList);true).
checkLambda(_,_,[]):- (sDebugMode -> writeln("Catch all.");true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% makePotList(+[InExpr],+Deepth,+(Iterator,[Group]),-Max_Group_Integer,+[LexExpr],-[Expr])
%
% NB: Nochmal auf "vorausgefüllten" Ausdrücken überprüfen
% NB: wie bei oberer Funktion auf Fall meherer möglicher Ausdrücke aus dem Lexikon erweitern (Stelligkeit/Valenz)
% This function checks if the input expression are already filled in by the lexical expression and if so, removes them from the list
% needs to be done per hand, because otherwise Prolog would substitude the variables of [B|RestB] for the expressions in [A|RestA]
% e.g. ['weis nix'] :: ([=d,v],[X,Out]>> (Out = weis(X,nix))), in this case "nix" is already filled in and neend not be considered by the rest of the Lexical search
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

makePotList([],_,(IterG,_),IterG,[],[]).
makePotList([A|RestA],T,(IterG,G),GOut,[B|RestB],PotExpr):-
  nonvar(A), var(B),  % check if there is a variabel on that place in the Lambda-Fkt
    append(G,[IterG],G1),NewIterG is IterG + 1,
    makePotList(RestA,T,(NewIterG,G),GOut,RestB,DeeperPot),append([(A,T,G1)],DeeperPot,PotExpr); % hier Teilen sich die Gruppen auf
  nonvar(A),nonvar(B),
    A =..[NameA|InputA], B =.. [NameB|InputB], % there is not a variable on that place in the Lambda-Fkt
    NameA == NameB,
      T1 is T + 1,                                                                                           % NB: Fehlerbehandlung einfügen, wenn es hier gebricht
      makePotList(InputA,T1,(IterG,G),G1Out,InputB,InputPot), % geht tiefer in den Ausdruck, ohne Semantisch Tiefer zu werden
      NewIterG is G1Out + 1, % Um zu vermeiden, dass zwei LI die gleiche Tiefe und Gruppierung haben, wird hier sichergestellt, dass die Li im Schwesterast ein fortlaufende Gruppennummer NACH diesem Ast haben.
      makePotList(RestA,T,(NewIterG,G),GOut,RestB,DeeperPot), % hier Teilen sich die Gruppen auf
      append(InputPot,DeeperPot,PotExpr);
  makePotList(RestA,T,(IterG,G),GOut,RestB,PotExpr).                                                                                         % NB: catch all, sollte mit der Fehlerbehandlung oben später abgestimmt werden



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% sortFromFkt(+[Lambda_list],+[Lambda_function_input],+[Pot_Expressions],-[sorted_Pot_Expressions])
%
% This function sorts the List of Potential Expressions according to their occurences in the Lambda-Expression of the Li
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
% lambdaFitting(+Expr_to_be_matched,+[Expr],-[Li]).
% Expr_ti_be_matched consists of an Expression, an integer for the depth of the expression and a list of the group it belongs to
% This functions checks the first expression and if it can be matched to a Lambda-expression in the lexicon adds the corresponding Li to the output-List
% potential further expressions are added to [Expr]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lambdaFitting([],[],[]).
lambdaFitting((In,T,G),Expr,Out):-
    In =.. InputFkt, checkLambda(InputFkt,T,G,PotExprList,Li),append(Expr,PotExprList,NewExprList), (NewExprList = [NewIn|Rest]; NewIn = [],Rest = []), lambdaFitting(NewIn,Rest,DeeperOut),
      append(DeeperOut,[Li],Out);    % Version, mit first in, last out
    %  Out = [Li|DeeperOut];           % Version mit first in first out, kann zu vertauschung führen
    Expr = [NewIn|Rest],lambdaFitting(NewIn,Rest,Out). % Ist der Teil hier legal? Unter welchen Umständen lass ich einen Ausdruck zu, der nicht mit einem Lambda aus dem Lexikon gepaart werden kann?
lambdaFitting(_,_,[]).
% und(eins,zwei) =.. [Name|Rest],length(Rest,Arg),functor(C,Name,Arg),W::(Fs,L>> _ = C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkLambda(+Expression,+Deepth,+[Group], -[PotExprList], -Li)
%
% NB: erweitere die Funktion später um eine Liste an möglichen Ausdrücken rauszufinden und weiterzugeben (Stelligkeit/Valenz)
% This function tries to unifiy the input expression with one of the lexical items, according to the lambda expression.
% It furthermore identifies potential expressions to be checked inside the expression under investigation.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


checkLambda([Name],T,G,PotExprList,Li) :- W::(Fs,Name), Li = lambdaLi(W,Fs,Name,T,G),PotExprList=[].  % Expression is a constant
checkLambda([Name|Input],T,G,PotExprList,Li):-                                                           % Expression is a function
    length(Input,Length),length(Rest,Length),
    FktLambda =..[Name|Rest],W::(Fs,L >> (Out = FktLambda)),Li = lambdaLi(W,Fs,L >> (Out = FktLambda),T,G), T2 is T + 1,
    makePotList(Input,T2,(1,G),_,Rest,PotExpr), % geht Tiefer in die Semantik
    sortFromFkt(L,Rest,PotExpr,PotExprRe), % sort by occurence in the Lambda-function
    reverse(PotExprRe,PotExprList). % reverse, because FILO

checkLambda(_,_,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% makePotList(+[InExpr],+Deepth,+(Iterator,[Group]),-Max_Group_Integer,+[LexExpr],-[Expr])
%
% NB: Nochmal auf "vorausgefüllten" Ausdrücken überprüfen
% NB: wie bei oberer Funktion auf Fall meherer möglicher Ausdrücke aus dem Lexikon erweitern (Stelligkeit/Valenz)
% This function checks if the input expression are already filled in by the lexical expression and if so, removes them from the list
% needs to be done per hand, because otherwise Prolog would substitude the variables of [B|RestB] for the expressions in [A|RestA]
% e.g. ['weis nix'] :: ([=d,v],[X,Out]>> (Out = weis(X,nix))), in this case "nix" is already filled in and neend not be considered by the rest of the Lexical search
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

makePotList([],_,(IterG,_),IterG,[],[]).
makePotList([A|RestA],T,(IterG,G),GOut,[B|RestB],PotExpr):-
  nonvar(A), var(B),  % check if there is a variabel on that place in the Lambda-Fkt
    append(G,[IterG],G1),NewIterG is IterG + 1,
    makePotList(RestA,T,(NewIterG,G),GOut,RestB,DeeperPot),append([(A,T,G1)],DeeperPot,PotExpr); % hier Teilen sich die Gruppen auf
  nonvar(A),nonvar(B),
    A =..[NameA|InputA], B =.. [NameB|InputB], % there is not a variable on that place in the Lambda-Fkt
    NameA == NameB,
      T1 is T + 1,                                                                                           % NB: Fehlerbehandlung einfügen, wenn es hier gebricht
      makePotList(InputA,T1,(IterG,G),G1Out,InputB,InputPot), % geht tiefer in den Ausdruck, ohne Semantisch Tiefer zu werden
      NewIterG is G1Out + 1, % Um zu vermeiden, dass zwei LI die gleiche Tiefe und Gruppierung haben, wird hier sichergestellt, dass die Li im Schwesterast ein fortlaufende Gruppennummer NACH diesem Ast haben.
      makePotList(RestA,T,(NewIterG,G),GOut,RestB,DeeperPot), % hier Teilen sich die Gruppen auf
      append(InputPot,DeeperPot,PotExpr);
  makePotList(RestA,T,(IterG,G),GOut,RestB,PotExpr).                                                                                         % NB: catch all, sollte mit der Fehlerbehandlung oben später abgestimmt werden



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
