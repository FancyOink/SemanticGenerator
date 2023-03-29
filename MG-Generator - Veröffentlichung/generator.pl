% file: generator.pl
% origin author : J. Kuhn
% origin date: May 2021
% purpose: generates sentances according to provided list of LI

:- op(500, xfy, ::). % infix predicate for lexical items
:- op(500, fx, =). % for selection features

:- ['grammars/maus'].

%:- ['listening/enrich'].




/*
NB: -> Take LI
    -> use merge/move for as long as possible
    -> take next LI, if possible
    -> repeat until no LI is left
    -> check derivation tree

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
% NB: LI mehr aufdrösseln um Paternmatching mit Selektor und Kategorie einzufügen
% NB: SMC implementieren
% worspace(+Li-Liste,+Workspace,-Derivation Tree)
% Worspace works with FIFO, if two items have the same category
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
workspace([],WSpace,DT) :- nl,writeln('final round through the workspace'),mg_function([],WSpace,PotDT), checkSpace(PotDT, DT). % Keine LIs in der Liste (mehr) -> MG-funktionen probieren wie geht und überprüfen, ob EIN Baum entsteht (-> Dann zu verify)
workspace([LI|LIs],[],DT) :- nl,writeln('first round through the workspace'),mg_function([],[LI],WSpace), workspace(LIs,WSpace,DT). % Noch kein Workspace -> erste LI rein und Workspace erstellen -> weiter iterieren
workspace([LI|LIs],WSpace,DT) :- nl,writeln('nth round through the workspace'),append(WSpace,[LI],NewList), mg_function([],NewList,NewSpace),workspace(LIs,NewSpace,DT). % LI in bestehende Workspace rein -> MG-Funktionen an nun erweiterten Workspace probieren wie geht -> weiter iterieren
workspace(_,_,_) :- nl, writeln('Workspace not fitting').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% merge   -> tree([([W],[Fs])],Subtree1,Subtree2)
% move    -> tree([([W],[Fs])],empty   ,Subtree)
% LexItem ->   li( [W],  [Fs])
%NB: - SMC implementieren
%    - Lambda einfügen
% mg_function(+no_match_Work_space, +Workspace_to_be_worked_on, -Derivation_Tree(s)_as_far_as_possible)
% probiert die verschiedenen MG-Funktionen nach möglichkeit am Workspace (FIFO bei Gleichstand (merge)/ SMC(Move))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mg_function(WS,[],WS):- writeln('found final WS: '), writeln(WS). % empty worked_on_WS -> no_match_WS = NewWS
mg_function([],[li(W,Fs)],[li(W,Fs)]):- writeln('only one Item: '), writeln(li(W,Fs)). % only one LI -> only one LI
mg_function(NoMatWS,[tree([(W,[+X|Fs])|Chains],Subl, Subr)|Items], NewWS) :- nl,writeln('TryMove Licensor'), write(NoMatWS), nl,writeln([tree([(W,[+X|Fs])|Chains],Subl, Subr)|Items]),tryMoveSor(NoMatWS,tree([(W,[+X|Fs])|Chains],Subl, Subr),Items, [], NewWS).  % Tree and +X -> try move
mg_function(NoMatWS,[tree([(W,[-X|Fs])|Chains],Subl, Subr)|Items], NewWS) :- nl,writeln('TryMove Licensee'), write(NoMatWS), nl,writeln([tree([(W,[-X|Fs])|Chains],Subl, Subr)|Items]),tryMoveSee(NoMatWS,tree([(W,[-X|Fs])|Chains],Subl, Subr),Items, NewWS).      % Tree and -X -> try move
mg_function(NoMatWS,[tree([(W,[=X|Fs])|Chains],Subl, Subr)|Items], NewWS) :- nl,writeln('TryMerge Selector DI'),write(NoMatWS),  nl,writeln([tree([(W,[=X|Fs])|Chains],Subl, Subr)|Items]),tryMergeSel(NoMatWS,tree([(W,[=X|Fs])|Chains],Subl, Subr),Items,[],NewWS).   % Tree and =X -> try merge
mg_function(NoMatWS,[tree([(W,[ X|Fs])|Chains],Subl, Subr)|Items], NewWS) :- nl,writeln('TryMerge Category DI'), write(NoMatWS), nl,writeln([tree([(W,[ X|Fs])|Chains],Subl, Subr)|Items]),tryMergeCat(NoMatWS,tree([(W,[ X|Fs])|Chains],Subl, Subr),Items,[],NewWS).   % Tree and  X -> try merge
mg_function(NoMatWS,[li(W,[=X|Fs])|Items], NewWS) :- nl,writeln('TryMerge Selector LI'), write(NoMatWS), nl, writeln([li(W,[=X|Fs])|Items]),tryMergeSel(NoMatWS,li(W,[=X|Fs]),Items,[],NewWS). % LI and =X -> try merge
mg_function(NoMatWS,[li(W,[ X|Fs])|Items], NewWS) :- nl,writeln('TryMerge category LI'),write(NoMatWS),  nl, writeln([li(W,[ X|Fs])|Items]),tryMergeCat(NoMatWS,li(W,[ X|Fs]),Items,[],NewWS). % LI and  X -> try merge
mg_function(_,_,_) :- nl, writeln( 'no matching Element for MG Functions').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Try functions for move and merge, separated acording to which feature triggerd them
% tryMerge triggered by Selektor features
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tryMergeSel(NoMatWS,li(W,[=X|Fs]), [li(V,[ X])|Items], NoFit, NewWS) :-                                                                                      % Li/Li merge1
                                                  nl, write('TryMerge Selector LI/LI merge1'),
                                                  merge1( li(W,[=X|Fs]), li(V,[X]),NewTree),
                                                  nl, writeln(NewTree),
                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeSel(NoMatWS,li(W,[=X|Fs]), [li(V,[ X|Fps])|Items], NoFit, NewWS) :-                                                                                  % Li/Li merge3
                                                  nl, write('TryMerge Selector LI/LI merge3'),
                                                  merge3( li(W,[=X|Fs]), li(V,[X|Fps]),NewTree),
                                                  nl, writeln(NewTree),
                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeSel(NoMatWS,li(W,[=X|Fs]),[tree([(V,[ X])|Chain2s],Sub2l, Sub2r)|Items], NoFit,NewWS) :-                                                             % Li/Di merge1
                                                  nl, write('TryMerge Selector LI/DI merge1'),
                                                  merge1( li(W,[=X|Fs]), tree([(V,[X])|Chain2s],Sub2l, Sub2r), NewTree),
                                                  nl, writeln(NewTree),
                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeSel(NoMatWS,li(W,[=X|Fs]), [tree([(V,[ X|Fps])|Chain2s],Sub2l, Sub2r)|Items], NoFit,NewWS) :-                                                        % Li/Di merge3
                                                  nl, write('TryMerge Selector LI/DI merge3'),
                                                  merge3( li(W,[=X|Fs]), tree([(V,[X|Fps])|Chain2s],Sub2l, Sub2r), NewTree),
                                                  nl, writeln(NewTree),
                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeSel(NoMatWS,tree([(W,[=X|Fs])|Chains],Subl, Subr), [li(V,[ X])|Items], NoFit, NewWS) :-                                                              % Di/Li merge2
                                                  nl, write('TryMerge Selector DI/LI merge2'),
                                                  merge2( tree([(W,[=X|Fs])|Chains],Subl, Subr), li(V,[X]),NewTree),
                                                  nl, writeln(NewTree),
                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeSel(NoMatWS,tree([(W,[=X|Fs])|Chains],Subl, Subr), [li(V,[ X|Fps])|Items], NoFit, NewWS) :-                                                          % Di/Li merge3
                                                  nl, write('TryMerge Selector DI/LI merge3'),
                                                  merge3( tree([(W,[=X|Fs])|Chains],Subl, Subr), li(V,[X|Fps]),NewTree),
                                                  nl, writeln(NewTree),
                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeSel(NoMatWS,tree([(W,[=X|Fs])|Chains],Subl, Subr), [tree([(V,[ X])|Chain2s],Sub2l, Sub2r)|Items], NoFit, NewWS) :-                                   % Di/Di merge2
                                                  nl, write('TryMerge Selector DI/DI merge2'),
                                                  merge2( tree([(W,[=X|Fs])|Chains],Subl, Subr), tree([(V,[X])|Chain2s],Sub2l, Sub2r), NewTree),
                                                  nl, writeln(NewTree),
                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeSel(NoMatWS,tree([(W,[=X|Fs])|Chains],Subl, Subr), [tree([(V,[ X|Fps])|Chain2s],Sub2l, Sub2r)|Items], NoFit, NewWS) :-                               % Di/Di merge3
                                                  nl, write('TryMerge Selector DI/DI merge3'),
                                                  merge3( tree([(W,[=X|Fs])|Chains],Subl, Subr), tree([(V,[X|Fps])|Chain2s],Sub2l, Sub2r), NewTree),
                                                  nl, writeln(NewTree),
                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeSel(NoMatWS,Selektor,[],WS, NewWS) :- nl, writeln('TryMerge Selector No Match, try other Feature in WS'), append(NoMatWS,[Selektor],NewNoMatWS), mg_function(NewNoMatWS,WS,NewWS).                                       % no match found in the whole work space, try if other feature pairs can be found in the rest of the work space
tryMergeSel(NoMatWS,Selektor, [Element| Items],  NoFit, NewWS) :- nl,writeln('TryMerge Selector No Match, try matching with other Item in WS'),append(NoFit, [Element], NewNoFit), tryMergeSel(NoMatWS, Selektor, Items, NewNoFit, NewWS).         % next item in work space not a match, try the next one
tryMergeSel(_,_,_,_,_) :- nl, writeln('Somehow wrong input in tryMergeSel').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tryMerge triggered by Category feature
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tryMergeCat(NoMatWS,li(W,[X]), [li(V,[=X|Fps])|Items], NoFit, NewWS) :-                                                                                            % Li/Li merge1
                                                  nl, write('TryMerge Category LI/LI merge1'),
                                                  merge1(li(V,[=X|Fps]),li(W,[ X]), NewTree),
                                                  nl, writeln(NewTree),
                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeCat(NoMatWS,li(W,[X| Fs]), [li(V,[=X|Fps])|Items], NoFit, NewWS) :-                                                                                        % Li/Li merge3
                                                  nl, write('TryMerge Category LI/LI merge3'),
                                                  merge3(li(V,[=X|Fps]), li(W,[ X| Fs]), NewTree),
                                                  nl, writeln(NewTree),
                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeCat(NoMatWS,li(W,[X]), [tree([(V,[=X|Fps])|Chains],Subl, Subr)|Items], NoFit, NewWS) :-                                                                    % Li/Di merge2
                                                  nl, write('TryMerge Category LI/DI merge2'),
                                                  merge2(tree([(V,[=X|Fps])|Chains],Subl, Subr),li(W,[ X]), NewTree),
                                                  nl, writeln(NewTree),
                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeCat(NoMatWS,li(W,[X| Fs]), [tree([(V,[=X|Fps])|Chains],Subl, Subr)|Items], NoFit, NewWS) :-                                                                % Li/Di merge3
                                                  nl, write('TryMerge Category LI/DI merge3'),
                                                  merge3(tree([(V,[=X|Fps])|Chains],Subl, Subr),li(W,[ X| Fs]), NewTree),
                                                  nl, writeln(NewTree),
                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeCat(NoMatWS,tree([(W,[X])|Chains],Subl, Subr), [li(V,[=X| Fps])|Items], NoFit, NewWS) :-                                                                   % Di/Li merge1
                                                  nl, write('TryMerge Category DI/LI merge1'),
                                                  merge1(li(V,[=X| Fps]), tree([(W,[ X])|Chains],Subl, Subr), NewTree),
                                                  nl, writeln(NewTree),
                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeCat(NoMatWS,tree([(W,[X| Fs])|Chains],Subl, Subr), [li(V,[=X| Fps])|Items], NoFit, NewWS) :-                                                               % Di/Li merge3
                                                  nl, write('TryMerge Category DI/LI merge3'),
                                                  merge3(li(V,[=X| Fps]),tree([(W,[ X| Fs])|Chains],Subl, Subr), NewTree),
                                                  nl, writeln(NewTree),
                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeCat(NoMatWS,tree([(W,[X])|Chains],Subl, Subr), [tree([(V,[=X|Fps])|Chain2s],Sub2l, Sub2r)|Items], NoFit, NewWS) :-                                         % Di/Di merge2
                                                  nl, write('TryMerge Category DI/DI merge2'),
                                                  merge2(tree([(V,[=X|Fps])|Chain2s],Sub2l, Sub2r), tree([(W,[ X])|Chains],Subl, Subr), NewTree),
                                                  nl, writeln(NewTree),
                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeCat(NoMatWS,tree([(W,[X| Fs])|Chains],Subl, Subr), [tree([(V,[=X|Fps])|Chain2s],Sub2l, Sub2r)|Items], NoFit, NewWS) :-                                     % Di/Di merge3
                                                  nl, write('TryMerge Category DI/DI merge3'),
                                                  merge3(tree([(V,[=X|Fps])|Chain2s],Sub2l, Sub2r), tree([(W,[ X| Fs])|Chains],Subl, Subr), NewTree),
                                                  nl, writeln(NewTree),
                                                  append([NewTree|NoFit], Items, WsTree),append(NoMatWS,WsTree,WholeWS), mg_function([],WholeWS,NewWS).
tryMergeCat(NoMatWS,Category,[],WS,NewWS) :- nl, writeln('TryMerge Category No Match, try other Feature in WS'),append(NoMatWS,[Category],NewNoMatWS), write(NewNoMatWS),nl,writeln(WS),mg_function(NewNoMatWS,WS,NewWS).                                               % no match found in the whole work space, try if other feature pairs can be found in the rest of the work space
tryMergeCat(NoMatWS,Category, [Element| Items], NoFit, NewWS) :- nl,writeln('TryMerge Category No Match, try matching with other Item in WS'), append(NoFit, [Element], NewNoFit), tryMergeCat(NoMatWS, Category, Items, NewNoFit, NewWS).                        % next item in work space not a match, try the next one
tryMergeCat(_,_,_,_,_) :- nl, writeln('Somehow wrong input in tryMergeCat').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tryMove triggered by Licensor features
% NB: SMC implementieren
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tryMoveSor(NoMatWS,tree([(W,[+X| Fs]),(V,[-X])|Chains], Subl, Subr), Items, NoFit,NewWS) :-                                                                         % move1
                                                  nl, write('TryMove Licensor move1'),
                                                  move1(tree([(W,[+X| Fs]),(V,[-X])|Chains], Subl, Subr), NoFit, NewTree),
                                                  nl, writeln(NewTree),
                                                  append(NoMatWS,[NewTree|Items],WholeWS), mg_function([],WholeWS, NewWS).
tryMoveSor(NoMatWS,tree([(W,[+X| Fs]),(V,[-X| Fps])|Chains], Subl, Subr), Items, NoFit,NewWS) :-                                                                    % move2
                                                  nl, write('TryMove Licensor move2'),
                                                  move2(tree([(W,[+X| Fs]),(V,[-X| Fps])|Chains], Subl, Subr), NoFit, NewTree),
                                                  nl, writeln(NewTree),
                                                  append(NoMatWS,[NewTree|Items],WholeWS), mg_function([],WholeWS, NewWS).
tryMoveSor(NoMatWS,tree([Licensor], Subl, Subr), Items, NoFit, NewWS) :- nl, writeln('TryMove Licensor No Match, try other Feature in WS'),append(NoMatWS, [tree([Licensor|NoFit], Subl, Subr)], NewNoMatWS), mg_function(NewNoMatWS,Items,NewWS).       % no match found in the whole chain, try if other feature pairs can be found in the rest of the work space
tryMoveSor(NoMatWS,tree([Licensor,NotLicensee|Chains], Subl, Subr), Items, NoFit, NewWS) :-                                                                         %  next item in chain not a match, try the next one
                                                  nl,writeln('TryMove Licensor No Match, try matching with other Item in WS'),append(NoFit, [NotLicensee], NewNoFit), nl, writeln([NoMatWS,tree([Licensor|Chains], Subl, Subr), Items, NewNoFit, NewWS]), tryMoveSor(NoMatWS,tree([Licensor|Chains], Subl, Subr), Items, NewNoFit, NewWS).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tryMove triggered by Licensee feature, This is just because of completion. This should never occur, or be cought by the checkSpace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tryMoveSee(NoMatWS,Licensee,Items,NewWS) :- nl,writeln('TryMove Licensee You made a bad LI to reach this'),append(NoMatWS,[Licensee],NewNoMatWS), mg_function(NewNoMatWS,Items,NewWS).                                                                  % Licensees as the head feature are dead ends, let's see if the checkSpace catches this one

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the actual MG functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

merge1(li(W,[=X|Fs]),li(V,[ X]), NewTree) :-
    append(W,V,WV), NewTree = tree([(WV,Fs)], li(W,[=X|Fs]), li(V,[ X])).
merge1(li(W,[=X|Fs]), tree([(V,[X])|Chains],Sub2l, Sub2r), NewTree) :-
    append(W,V,WV), NewTree = tree([(WV,Fs)|Chains], li(W,[=X|Fs]), tree([(V,[X])|Chains],Sub2l, Sub2r)).

merge2(tree([(W,[=X|Fs])|Chains],Subl, Subr), li(V,[X]), NewTree) :-
    append(V,W,VW), NewTree = tree([(VW,Fs)| Chains], tree([(W,[=X|Fs])|Chains],Subl, Subr), li(V,[X]) ).
merge2(tree([(W,[=X|Fs])|Chains],Subl, Subr), tree([(V,[X])|Chain2s],Sub2l, Sub2r), NewTree) :-
    append(V,W,VW), append(Chains, Chain2s, NewChains), NewTree = tree([(VW,Fs)|NewChains], tree([(W,[=X|Fs])|Chains],Subl, Subr), tree([(V,[X])|Chain2s],Sub2l, Sub2r)).


merge3(li(W,[=X|Fs]), li(V,[ X| Fps]), tree([(W,Fs), (V,Fps)], li(W,[=X|Fs]), li(V,[ X| Fps]))).
merge3(li(W,[=X|Fs]), tree([(V,[X|Fps])|Chain2s],Sub2l, Sub2r), tree([(W,Fs),(V,Fps)| Chain2s], li(W,[=X|Fs]), tree([(V,[X|Fps])|Chain2s], Sub2l, Sub2r))).
merge3(tree([(W,[=X|Fs])|Chains],Subl, Subr), li(V,[ X| Fps]), tree([(W,Fs), (V,Fps)| Chains], tree([(W,[=X|Fs])|Chains],Subl, Subr), li(V,[ X| Fps]))).
merge3(tree([(W,[=X|Fs])|Chains],Subl, Subr), tree([(V,[X|Fps])|Chain2s],Sub2l, Sub2r), NewTree):-
    append([(W,Fs)|Chains],[(V,Fps)|Chain2s], NewItem), NewTree = tree(NewItem, tree([(W,[=X|Fs])|Chains],Subl, Subr), tree([(V,[X|Fps])|Chain2s],Sub2l, Sub2r)).

move1(tree([(W,[+X| Fs]),(V,[-X])|Chains], Subl, Subr), NoFit, NewTree):-
    nl,writeln('Go Move1'),append(V,W,VW), nl,writeln('first append'),write('Nofit: '),writeln(NoFit),write('Chains: '),writeln(Chains),append(NoFit,Chains,NewChains), nl,writeln('second append'),append([(W,[+X| Fs])|NoFit], [(V,[-X])|Chains], OldTree), nl,writeln('mad eldTree'),NewTree = tree([(VW,Fs)|NewChains], empty, tree(OldTree, Subl, Subr)), nl,writeln('made NewTree').
move1(tree([(W,[+X| Fs]),(V,[-X])], Subl, Subr), NoFit, NewTree):-
    append(V,W,VW), append([(W,[+X| Fs])|NoFit], [(V,[-X])], OldTree), NewTree = tree([(VW,Fs)|NoFit], empty, tree(OldTree, Subl, Subr)).

move2(tree([(W,[+X| Fs]),(V,[-X| Fps])|Chains], Subl, Subr), NoFit, NewTree):-
    append([(W,Fs)|NoFit], [(V,Fps)|Chains], NewItem), append([(W,[+X| Fs])|NoFit],[(V,[-X| Fps])|Chains], OldTree), NewTree = tree(NewItem,empty, tree(OldTree, Subl, Subr)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkSpace(+Deriavtion_Tree_to_be_checked,-legal_Derivation_Tree)
% Überprüft, ob workspace nur noch aus einem Ableitungsbaum besteht
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%checkSpace([],[]).
checkSpace([WS],WS).

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
