% file: lambdaWorkspace.pl
% origin author : J. Kuhn
% origin date: May 2021
% purpose: generates sentances according to provided list of LI

:- op(500, xfy, ::). % infix predicate for lexical items
:- op(500, fx, =). % for selection features
%:- op(500, fx, *). % for category features
%:- ['listening/enrich'].




/*
NB: -> Take LI
    -> use merge/move for as long as possible
    -> take next LI, if possible
    -> repeat until no LI is left
    -> check derivation tree
NB: If in the future different types of MGs are to be implemented, use Dicts-structure for tree and li
NB: Sonderfall, dass nur ein Item eingefügt wird mal lösen
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% generate([LI-Eingabeliste],[Ableitungsbaum]).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate([],[]).
generate(LI,DT) :-
    workspace(LI,[],DT).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% worspace(+Li-Liste,+Workspace,-Derivation Tree)
% Worspace works with FIFO, if two items have the same category
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
workspace([],WSpace,DT) :- mg_function([],WSpace,PotDT), checkSpace(PotDT, DT). % Keine LIs in der Liste (mehr) -> MG-funktionen probieren wie geht und überprüfen, ob EIN Baum entsteht (-> Dann zu verify)
workspace([LI|LIs],[],DT) :- mg_function([],[LI],WSpace), workspace(LIs,WSpace,DT). % Noch kein Workspace -> erste LI rein und Workspace erstellen -> weiter iterieren
workspace([LI|LIs],WSpace,DT) :- append(WSpace,[LI],NewList), mg_function([],NewList,NewSpace),workspace(LIs,NewSpace,DT). % LI in bestehende Workspace rein -> MG-Funktionen an nun erweiterten Workspace probieren wie geht -> weiter iterieren
workspace(_,_,_) .
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% merge   -> tree([([W],[Fs],L)],MGFunc,Subtree1,Subtree2)
% move    -> tree([([W],[Fs],L)],MGFunc,empty   ,Subtree)
% LexItem ->   li( [W],  [Fs], L)
%
% NB: Category mehr abgrenzen
% mg_function(+no_match_Work_space, +Workspace_to_be_worked_on, -Derivation_Tree(s)_as_far_as_possible)
% probiert die verschiedenen MG-Funktionen nach möglichkeit am Workspace (FIFO bei Gleichstand (merge)/ SMC(Move))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mg_function(WS,[],WS). % empty worked_on_WS -> no_match_WS = NewWS
mg_function([],[li(W,Fs,L)],[li(W,Fs,L)]). % only one LI -> only one LI
mg_function(NoMatWS,[tree([(W,[+X|Fs],L)|Chains],MGFunc, Subl, Subr)|Items], NewWS) :- tryMoveSor(NoMatWS,tree([(W,[+X|Fs],L)|Chains],MGFunc, Subl, Subr),Items, [], NewWS).  % Tree and +X -> try move
mg_function(NoMatWS,[tree([(W,[-X|Fs],L)|Chains],MGFunc, Subl, Subr)|Items], NewWS) :- tryMoveSee(NoMatWS,tree([(W,[-X|Fs],L)|Chains],MGFunc, Subl, Subr),Items, NewWS).      % Tree and -X -> try move
mg_function(NoMatWS,[tree([(W,[=X|Fs],L)|Chains],MGFunc, Subl, Subr)|Items], NewWS) :- tryMergeSel(NoMatWS,tree([(W,[=X|Fs],L)|Chains],MGFunc, Subl, Subr),Items,[],NewWS).   % Tree and =X -> try merge
mg_function(NoMatWS,[tree([(W,[ X|Fs],L)|Chains],MGFunc, Subl, Subr)|Items], NewWS) :- tryMergeCat(NoMatWS,tree([(W,[ X|Fs],L)|Chains],MGFunc, Subl, Subr),Items,[],NewWS).   % Tree and  X -> try merge
mg_function(NoMatWS,[li(W,[=X|Fs],L)|Items], NewWS) :- tryMergeSel(NoMatWS,li(W,[=X|Fs],L),Items,[],NewWS). % LI and =X -> try merge
mg_function(NoMatWS,[li(W,[ X|Fs],L)|Items], NewWS) :- tryMergeCat(NoMatWS,li(W,[ X|Fs],L),Items,[],NewWS). % LI and  X -> try merge
mg_function(_,_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Try functions for move and merge, separated acording to which feature triggerd them
% tryMerge triggered by Selektor features
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tryMergeSel(NoMatWS,li(W,[=X|Fs],L), [li(V,[ X],M)|Items], NoFit, NewWS) :-                                                                                      % Li/Li merge1

                                                  merge1( li(W,[=X|Fs],L), li(V,[X],M),NewTree),

                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeSel(NoMatWS,li(W,[=X|Fs],L), [li(V,[ X|Fps],M)|Items], NoFit, NewWS) :-                                                                                  % Li/Li merge3

                                                  merge3( li(W,[=X|Fs],L), li(V,[X|Fps],M),NewTree),

                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeSel(NoMatWS,li(W,[=X|Fs],L),[tree([(V,[ X],M)|Chain2s],MGFunc, Sub2l, Sub2r)|Items], NoFit,NewWS) :-                                                             % Li/Di merge1

                                                  merge1( li(W,[=X|Fs],L), tree([(V,[X],M)|Chain2s],MGFunc, Sub2l, Sub2r), NewTree),

                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeSel(NoMatWS,li(W,[=X|Fs],L), [tree([(V,[ X|Fps],M)|Chain2s],MGFunc, Sub2l, Sub2r)|Items], NoFit,NewWS) :-                                                        % Li/Di merge3

                                                  merge3( li(W,[=X|Fs],L), tree([(V,[X|Fps],M)|Chain2s],MGFunc, Sub2l, Sub2r), NewTree),

                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeSel(NoMatWS,tree([(W,[=X|Fs],L)|Chains],MGFunc, Subl, Subr), [li(V,[ X],M)|Items], NoFit, NewWS) :-                                                              % Di/Li merge2

                                                  merge2( tree([(W,[=X|Fs],L)|Chains],MGFunc, Subl, Subr), li(V,[X],M),NewTree),

                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeSel(NoMatWS,tree([(W,[=X|Fs],L)|Chains],MGFunc, Subl, Subr), [li(V,[ X|Fps],M)|Items], NoFit, NewWS) :-                                                          % Di/Li merge3

                                                  merge3( tree([(W,[=X|Fs],L)|Chains],MGFunc, Subl, Subr), li(V,[X|Fps],M),NewTree),

                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeSel(NoMatWS,tree([(W,[=X|Fs],L)|Chains],MGFunc1, Subl, Subr), [tree([(V,[ X],M)|Chain2s],MGFunc2, Sub2l, Sub2r)|Items], NoFit, NewWS) :-                                   % Di/Di merge2

                                                  merge2( tree([(W,[=X|Fs],L)|Chains],MGFunc1, Subl, Subr), tree([(V,[X],M)|Chain2s],MGFunc2, Sub2l, Sub2r), NewTree),

                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeSel(NoMatWS,tree([(W,[=X|Fs],L)|Chains],MGFunc1, Subl, Subr), [tree([(V,[ X|Fps],M)|Chain2s],MGFunc2, Sub2l, Sub2r)|Items], NoFit, NewWS) :-                               % Di/Di merge3

                                                  merge3( tree([(W,[=X|Fs],L)|Chains],MGFunc1, Subl, Subr), tree([(V,[X|Fps],M)|Chain2s],MGFunc2, Sub2l, Sub2r), NewTree),

                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeSel(NoMatWS,Selektor,[],WS, NewWS) :- append(NoMatWS,[Selektor],NewNoMatWS), mg_function(NewNoMatWS,WS,NewWS).                                       % no match found in the whole work space, try if other feature pairs can be found in the rest of the work space
tryMergeSel(NoMatWS,Selektor, [Element| Items],  NoFit, NewWS) :- append(NoFit, [Element], NewNoFit), tryMergeSel(NoMatWS, Selektor, Items, NewNoFit, NewWS).         % next item in work space not a match, try the next one
tryMergeSel(_,_,_,_,_).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tryMerge triggered by Category feature
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tryMergeCat(NoMatWS,li(W,[X],L), [li(V,[=X|Fps],M)|Items], NoFit, NewWS) :-                                                                                            % Li/Li merge1

                                                  merge1(li(V,[=X|Fps],M),li(W,[ X],L), NewTree),

                                                  append(NoFit, [NewTree|Items], WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeCat(NoMatWS,li(W,[X| Fs],L), [li(V,[=X|Fps],M)|Items], NoFit, NewWS) :-                                                                                        % Li/Li merge3

                                                  merge3(li(V,[=X|Fps],M), li(W,[ X| Fs],L), NewTree),

                                                  append(NoFit, [NewTree|Items], WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeCat(NoMatWS,li(W,[X],L), [tree([(V,[=X|Fps],M)|Chains],MGFunc, Subl, Subr)|Items], NoFit, NewWS) :-                                                                    % Li/Di merge2

                                                  merge2(tree([(V,[=X|Fps],M)|Chains],MGFunc, Subl, Subr),li(W,[ X],L), NewTree),

                                                  append(NoFit, [NewTree|Items], WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeCat(NoMatWS,li(W,[X| Fs],L), [tree([(V,[=X|Fps],M)|Chains],MGFunc, Subl, Subr)|Items], NoFit, NewWS) :-                                                                % Li/Di merge3

                                                  merge3(tree([(V,[=X|Fps],M)|Chains],MGFunc, Subl, Subr),li(W,[ X| Fs],L), NewTree),

                                                  append(NoFit, [NewTree|Items], WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeCat(NoMatWS,tree([(W,[X],L)|Chains],MGFunc, Subl, Subr), [li(V,[=X| Fps],M)|Items], NoFit, NewWS) :-                                                                   % Di/Li merge1

                                                  merge1(li(V,[=X| Fps],M), tree([(W,[ X],L)|Chains],MGFunc, Subl, Subr), NewTree),

                                                  append(NoFit, [NewTree|Items], WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeCat(NoMatWS,tree([(W,[X| Fs],L)|Chains],MGFunc, Subl, Subr), [li(V,[=X| Fps],M)|Items], NoFit, NewWS) :-                                                               % Di/Li merge3

                                                  merge3(li(V,[=X| Fps],M),tree([(W,[ X| Fs],L)|Chains],MGFunc, Subl, Subr), NewTree),

                                                  append(NoFit, [NewTree|Items], WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeCat(NoMatWS,tree([(W,[X],L)|Chains],MGFunc1, Subl, Subr), [tree([(V,[=X|Fps],M)|Chain2s],MGFunc2, Sub2l, Sub2r)|Items], NoFit, NewWS) :-                                         % Di/Di merge2

                                                  merge2(tree([(V,[=X|Fps],M)|Chain2s],MGFunc2, Sub2l, Sub2r), tree([(W,[ X],L)|Chains],MGFunc1, Subl, Subr), NewTree),

                                                  append(NoFit, [NewTree|Items], WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeCat(NoMatWS,tree([(W,[X| Fs],L)|Chains],MGFunc1, Subl, Subr), [tree([(V,[=X|Fps],M)|Chain2s],MGFunc2, Sub2l, Sub2r)|Items], NoFit, NewWS) :-                                     % Di/Di merge3

                                                  merge3(tree([(V,[=X|Fps],M)|Chain2s],MGFunc2, Sub2l, Sub2r), tree([(W,[ X| Fs],L)|Chains],MGFunc1, Subl, Subr), NewTree),

                                                  append(NoFit, [NewTree|Items], WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeCat(NoMatWS,Category,[],WS,NewWS) :- append(NoMatWS,[Category],NewNoMatWS), mg_function(NewNoMatWS,WS,NewWS).                                               % no match found in the whole work space, try if other feature pairs can be found in the rest of the work space
tryMergeCat(NoMatWS,Category, [Element| Items], NoFit, NewWS) :-  append(NoFit, [Element], NewNoFit), tryMergeCat(NoMatWS, Category, Items, NewNoFit, NewWS).                        % next item in work space not a match, try the next one
tryMergeCat(_,_,_,_,_).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tryMove triggered by Licensor features
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tryMoveSor(NoMatWS,tree([(W,[+X| Fs],L),(V,[-X],M)|Chains],MGFunc, Subl, Subr), Items, NoFit,NewWS) :-                                                                         % move1
                                                  !,checkSMC(+X,Chains),

                                                  move1(tree([(W,[+X| Fs],L),(V,[-X],M)|Chains],MGFunc, Subl, Subr), NoFit, NewTree),

                                                  append(NoMatWS,[NewTree|Items],WholeWS), mg_function([],WholeWS, NewWS).
tryMoveSor(NoMatWS,tree([(W,[+X| Fs],L),(V,[-X| Fps],M)|Chains],MGFunc, Subl, Subr), Items, NoFit,NewWS) :-                                                                    % move2
                                                  !,checkSMC(+X,Chains),

                                                  move2(tree([(W,[+X| Fs],L),(V,[-X| Fps],M)|Chains],MGFunc, Subl, Subr), NoFit, NewTree),

                                                  append(NoMatWS,[NewTree|Items],WholeWS), mg_function([],WholeWS, NewWS).
tryMoveSor(NoMatWS,tree([Licensor], MGFunc, Subl, Subr), Items, NoFit, NewWS) :- append(NoMatWS, [tree([Licensor|NoFit],MGFunc, Subl, Subr)], NewNoMatWS), mg_function(NewNoMatWS,Items,NewWS).       % no match found in the whole chain, try if other feature pairs can be found in the rest of the work space
tryMoveSor(NoMatWS,tree([Licensor,NotLicensee|Chains],MGFunc , Subl, Subr), Items, NoFit, NewWS) :-                                                                         %  next item in chain not a match, try the next one
                                                  append(NoFit, [NotLicensee], NewNoFit), tryMoveSor(NoMatWS,tree([Licensor|Chains],MGFunc, Subl, Subr), Items, NewNoFit, NewWS).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tryMove triggered by Licensee feature, This is just because of completion. This should never occur, or be cought by the checkSpace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tryMoveSee(NoMatWS,Licensee,Items,NewWS) :- append(NoMatWS,[Licensee],NewNoMatWS), mg_function(NewNoMatWS,Items,NewWS).                                                                  % Licensees as the head feature are dead ends, let's see if the checkSpace catches this one

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the actual MG functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

merge1(li(W,[=X|Fs],L),li(V,[ X],M), NewTree) :-
    append(W,V,WV), makeLambda(L,M,N), NewTree = tree([(WV,Fs,N)], merge1, li(W,[=X|Fs],L), li(V,[ X],M)).
merge1(li(W,[=X|Fs],L), tree([(V,[X],M)|Chains],MGFunc, Sub2l, Sub2r), NewTree) :-
    append(W,V,WV), makeLambda(L,M,N), NewTree = tree([(WV,Fs,N)|Chains], merge1, li(W,[=X|Fs],L), tree([(V,[X],M)|Chains],MGFunc, Sub2l, Sub2r)).

merge2(tree([(W,[=X|Fs],L)|Chains],MGFunc, Subl, Subr), li(V,[X],M), NewTree) :-
    append(V,W,VW),  makeLambda(L,M,N), NewTree = tree([(VW,Fs,N)| Chains], merge2, tree([(W,[=X|Fs],L)|Chains],MGFunc, Subl, Subr), li(V,[X],M) ).
merge2(tree([(W,[=X|Fs],L)|Chains],MGFunc1, Subl, Subr), tree([(V,[X],M)|Chain2s],MGFunc2, Sub2l, Sub2r), NewTree) :-
    append(V,W,VW), makeLambda(L,M,N), append(Chains, Chain2s, NewChains), NewTree = tree([(VW,Fs,N)|NewChains], merge2, tree([(W,[=X|Fs],L)|Chains],MGFunc1, Subl, Subr), tree([(V,[X],M)|Chain2s],MGFunc2, Sub2l, Sub2r)).


merge3(li(W,[=X|Fs],L), li(V,[ X| Fps],M), tree([(W,Fs,L), (V,Fps,M)], merge3, li(W,[=X|Fs],L), li(V,[ X| Fps],M))).
merge3(li(W,[=X|Fs],L), tree([(V,[X|Fps],M)|Chain2s],MGFunc, Sub2l, Sub2r), tree([(W,Fs,L),(V,Fps,M)| Chain2s], merge3, li(W,[=X|Fs],L), tree([(V,[X|Fps],M)|Chain2s], MGFunc, Sub2l, Sub2r))).
merge3(tree([(W,[=X|Fs],L)|Chains],MGFunc, Subl, Subr), li(V,[ X| Fps],M), tree([(W,Fs,L), (V,Fps,M)| Chains], merge3, tree([(W,[=X|Fs],L)|Chains],MGFunc, Subl, Subr), li(V,[ X| Fps],M))).
merge3(tree([(W,[=X|Fs],L)|Chains],MGFunc1, Subl, Subr), tree([(V,[X|Fps],M)|Chain2s],MGFunc2, Sub2l, Sub2r), NewTree):-
    append([(W,Fs,L)|Chains],[(V,Fps,M)|Chain2s], NewItem), NewTree = tree(NewItem, merge3, tree([(W,[=X|Fs],L)|Chains],MGFunc1, Subl, Subr), tree([(V,[X|Fps],M)|Chain2s],MGFunc2, Sub2l, Sub2r)).

move1(tree([(W,[+X| Fs],L),(V,[-X],M)|Chains],MGFunc, Subl, Subr), NoFit, NewTree):-
    append(V,W,VW), makeLambda(L,M,N), append(NoFit,Chains,NewChains), append([(W,[+X| Fs],L)|NoFit], [(V,[-X],M)|Chains], OldTree), NewTree = tree([(VW,Fs,N)|NewChains],move1, empty, tree(OldTree,MGFunc, Subl, Subr)).
move1(tree([(W,[+X| Fs],L),(V,[-X],M)],MGFunc, Subl, Subr), NoFit, NewTree):-
    append(V,W,VW), makeLambda(L,M,N),  append([(W,[+X| Fs],L)|NoFit], [(V,[-X],M)], OldTree), NewTree = tree([(VW,Fs,N)|NoFit],move1, empty, tree(OldTree, MGFunc, Subl, Subr)).

move2(tree([(W,[+X| Fs],L),(V,[-X| Fps],M)|Chains],MGFunc, Subl, Subr), NoFit, NewTree):-
    append([(W,Fs,L)|NoFit], [(V,Fps,M)|Chains], NewItem), append([(W,[+X| Fs],L)|NoFit],[(V,[-X| Fps],M)|Chains], OldTree), NewTree = tree(NewItem,move2, empty, tree(OldTree,MGFunc, Subl, Subr)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkSpace(+Deriavtion_Tree_to_be_checked,-legal_Derivation_Tree)
% Checks if the Workspace realy only contains one derivation tree
% NB: funkt derzeit nicht, bzw. bringt keinen Abbruch hervor falls mehr als ein Baum übergeben wird
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkSpace([WS],WS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkSMC(+Selector,+Chain_to_be_checked)
% checks if the SMC holds
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkSMC(_,[]):- true.
checkSMC(+Selector,[(_,[-Selector|_],_)|_]):-
    !,false.
checkSMC(+Selector,[_|Chains]):-
  checkSMC(+Selector,Chains).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Lx.Ly (Y(X)) => MakeLambda(Y,X,Out)
% makeLambda(+Lambda1,+Lambda2,-NewLambda)
% applies an expression to a lambda abstraction
% Idee: cl(X>>Y),length(X,Vgl),Z=[a],length(Z,Vgl),A=..[call,X>>Y|Z],call(A);cl(X>>Y),Z=[a],NewCl =..[cl,X>>Y,Z].
% Die Auskommentierten Zeilen sind für eine Abstraktion in Abstraktion. Sind hier aber nach illegalem Schema implementiert
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%makeLambda(epsilon,M,M). % kommen für die Zukunft weg, da es kein bidirektional neutrales epsilon gibt
%makeLambda(L,epsilon,L). % wie oben
%makeLambda(cl(A>>B1,Z),E >> (Out = B2),L) :- append(A,E,NewA), length(NewA,Vgl), append(Z,[B2,L],NewZ), length(NewZ,Vgl), Ex =.. [call,NewA >> (Out = B1)|NewZ],call(Ex); append(Z,B2,NewZ), append(A,E,NewA),L = (cl(NewA>>(Out = B1),NewZ)).
%makeLambda(cl(A>>B1,Z1),cl(E>>(Out = B2),Z2),L) :- length(A,Vgl), append(Z1,[B2,LambdaOut],NewZ1),length(NewZ1,Vgl), Ex =..[call,A>>B1|NewZ1],call(Ex),append(E,[Out],NewE),L = cl(NewE>>(Out = LambdaOut),Z2); append(A,E,NewA),append(Z1,[B2|Z2],NewZ1), L = cl(NewA,B1,NewZ1).
%makeLambda(A>>B1,cl(E>>(Out = B2),Z),L) :- length(A,2),call(A>>B1,B2,LambdaOut),L = cl(E>> (Out = LambdaOut)); append(A,E,NewE),append([B2],Z,NewZ), L = cl(NewE>>(Out = B1),NewZ).
%makeLambda(A>>B1, E >> (Out = B2),L) :- length(A,2), call(A>>B1,B2,LambdaOut),L = E >> (Out = LambdaOut); append(A,E,NewA),L =.. [cl,NewA>>B1,B2].
makeLambda(cl(A>>B,Z),Y,LambdaOut) :- length(A, Vgl), append(Z,[Y,LambdaOut],NewZ),length(NewZ,Vgl), Ex =..[call,A>>B|NewZ], call(Ex); append(Z,[Y],NewZ),LambdaOut = (cl(A>>B,NewZ)).
makeLambda(A>>B,Y,LambdaOut) :- length(A,2), call(A>>B,Y,LambdaOut); LambdaOut =.. [cl,A>>B,[Y]].

%makeLambda(A,B,LambdaOut) :- append(A,B,LambdaOut).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Examples
%
% Maus frisst Käse und Katze frisst Maus
% generate([li(['Maus'],[d]),li(['Katze'],[d]),li([frisst],[=d,=d,v]),li(['Kaese'],[d]),li(['Maus'],[d]),li([frisst],[=d,=d,v]),li([und],[=c,=c,c]),li([],[=v,c]),li([],[=v,c])],DT).
%
%  e Maus frisst Kaese und e Katze frisst Maus
% generate([li(['Maus'],[d]),li(['Katze'],[d]),li([frisst],[=d,=d,v]),li(['Kaese'],[d]),li(['Maus'],[d]),li([frisst],[=d,=d,v]),li([und],[=c,=c,c]),li([e],[=v,c]),li([e],[=v,c])],DT).
%
% Katze frisst Maus
% generate([li(['Maus'],[d]),li(['Katze'],[d]),li([frisst],[=d,=d,v])],DT).
%
% achtunddreißig
% generate([li([],[=c1,+ssi,+zeh,+un,c1,-ssi]),li(['ßig'],[=c1,+ssi,cundZIG]),li([drei],[c1,-ssi,-zeh,-un]),li([],[=c1,+zi,c1]),li([acht],[c1,-zi,-zeh,-un]),li([],[=c1,+zeh,c1]),li([],[=c2,+taus_,c3,-taus]),li([],[=c3,+taus,c3]),li([],[=c3,c4]),li([und],[=cundZIG,=c1,+un,c2,-taus_])],DT).
%
% fünftausendvierzehn
% generate([li([],[=c1,+zi,c1]),li([vier],[c1,-zi,-zeh,-un]),li([],[=c1,+zeh,+un,c1,-zeh]),li([zehn],[=c1,+zeh,c2,-taus_]),li([],[=c2,+taus_,c3,-taus]),li([],[=c3,+taus,c3]),li([],[=c1,+zi,c1]),li(['fünf'],[c1,-zi,-zeh,-un]),li([],[=c1,+zeh,c1]),li([],[=c1,+un,c2,-taus_]),li([],[=c2,+taus_,c3,-taus]),li([tausend],[=c3,=c3,+taus,c4])],DT).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
