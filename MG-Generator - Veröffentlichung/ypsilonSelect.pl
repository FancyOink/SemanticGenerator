:- module(ypsilonSelect,[ypsilonSelectFkt/2]).
% file: ypsilonSelect.pl
% origin author : J. Kuhn
% origin date: August 2021
% purpose: selects ypsilon-LI from a Lexicon

%fDebugMode.
fDebugMode :- false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ypsilonSelectFkt(+[lambdaLi(W,Fs,Lambda,T,G)],-[li(W,Fs,Lambda)])
%
% This function inserts Epsilon-Li, if necessary.
% NB: mann könnte ein künstliches LI machen, welches nur die Startkategorie hat. Dies würde aber zu einem Merge mehr führen als üblich.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ypsilonSelectFkt(Input,Output):-
  checkFinishStart(Input,Status),                          % check if we are already finished
  ( isEmpty(Status,true),writeln("List is ok"),            % no Epsilon required, fast forward to Worspace
    changeToLI(Input,Output);
    (fDebugMode -> write("\nStatus: "), writeln(Status);true),
    prepList(Input,PrepInput),          % Prepare list for easier processing
    (fDebugMode -> write("\nPreped List: "), writeln(PrepInput);true),
    fillYpsilon(PrepInput,Status,FilledList), % Epsilon required, proceed the Fill-In
    (fDebugMode -> writeln("We have a full List!");true),
    flattenForWS(FilledList,FlattenList),       % final sorting, befor transfer to Workspace
    %checkFinishFace(FlattenList,FlatStatus),    % check if correctly filled
    %!,isEmpty(FlatStatus,true),
    changeToLI(FlattenList,Output)              % transform to true LI
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkFinishStart(+[List_LambdaLi],-[(Halting_FeatureRoot,Depth,Group),(Halting_FeatureLeaf,Depth,Group)|RestFetures)])
%
% This Function checks if finished at the start
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkFinishStart([],[]).
checkFinishStart(Input, Output):-
  checkFinishFace(Input,Checked),
  (Checked = [([A],_,_)],             % check if only the startcategory remains
    startCategory(A),
    Output = [];  % yes, all is finished
    Output = Checked).              % no, continue with filling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkFinishFace(+[List_LambdaLi],-[(Halting_FeatureRoot,Depth,Group),(Halting_FeatureLeaf,Depth,Group)|RestFetures)])
%
% This Function is a interface for the checkFinish function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkFinishFace([],[]).
checkFinishFace(Input,Output):-
  reverse(Input,RevInput),
  extractFList(RevInput,[(Head,D,G)|Check]),
  %reverse(Head,H),
  splitpos(Head,H),
  splitneg(Head,NegH),
  (fDebugMode -> write("\nFeature List to check: "), writeln([(H,D,G)|Check]);true), %Output= [(H,D,G)|Check].
  checkFinish((H,D,G),Check,[],OutputDown),
  (OutputDown = [([],_,_)],
  Output = [(NegH,D,G)];
  startCategory(A),
  NegH = [A|_],
  append(OutputDown,[(NegH,D,G)],Output);
  append([(NegH,D,G)],OutputDown,Output)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkDuringFillIn(+[List_LambdaLi],-[(Halting_FeatureRoot,Depth,Group),(Halting_FeatureLeaf,Depth,Group)|RestFetures)])
%
% This Function is a interface for the checkFinish function during Fill-In
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkDuringFillIn([],[]).
checkDuringFillIn(Input,Output):-
  (fDebugMode -> write("\nCheck status during fill-in.");true),
  flattenLambdaLi(Input,FlattenList),
  extractFList(FlattenList,[Head|RestLis]),
  splitHeadpos(Head,PositiveHead),
  (fDebugMode -> write("\nFeatures to check During: "), writeln([PositiveHead|RestLis]);true),
  checkFinish(PositiveHead,RestLis,[],Output).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkFinish(+Feature_List_Root,+[Feature_List_Leafs],+[buffer_to_be_checked],-Halting_Feature)
%
% This Function checks, if the group is finished, meaning all Features of the root can be matched with the Leafs
% NB: der Check, ob zwei Feature-Listen verglichen werden dürfen muss exakter werden
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkFinish(([],D,G),[],[],[([],D,G)]).                                         % finished, all is matched
checkFinish(Head,[],[],[Head]).                                              % finished, no input left
checkFinish(Head,[],Non,[Head|Non]).                        % Not Finished, output is list of remaining Features
checkFinish(([],_,_),Rest,[],Out):- filterEmpty(Rest,[],[H|ToCheck]),checkFinish(H,ToCheck,[],Out).
checkFinish(([],_,_),Rest,Non,Out):- append(Non,Rest,List), filterEmpty(List,[],[H|ToCheck]), checkFinish(H,ToCheck,[],Out). % NB: Wann kommt das hier zum Einsatz?
checkFinish(([HeadR|RootFs],RD,RG),[([HeadL|LeafFs],LD,LG)|RestLeafs],NonFit,OutF):-
  matchFeatures(HeadR,HeadL,true),append(NonFit,[(LeafFs,LD,LG)|RestLeafs],LeafListUnfiltered),filterEmpty(LeafListUnfiltered,[],LeafList),checkFinish((RootFs,RD,RG),LeafList,[],OutF);  % Feature matched, check next in List
  append(NonFit,[([HeadL|LeafFs],LD,LG)],LeafList),checkFinish(([HeadR|RootFs],RD,RG),RestLeafs,LeafList,OutF). % Feature did not match, check next one and put the other in for later

checkLegal([],_,no).
checkLegal(_,[],yes).
checkLegal(_,[(_,_,[])|_],no).
checkLegal([A|Rest1],[(F,D,[A|Rest2])|RestNon],no):- checkLegal(Rest1,[(F,D,Rest2)|RestNon],no).
checkLegal(LG,[_|RestNon],yes) :- checkLegal(LG,RestNon,yes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% extractFList(+[lambdaLi(W,Fs,Lambda,Depth,Group)|[RestLambdaLi]],-[[FeatureList_Li_List]])
%
% This function excerts the feature lists from the List of Lis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extractFList([],[]).
extractFList([lambdaLi(_,Fs,_,D,G)|RestLambdaLi],Out):-
  extractFList(RestLambdaLi,OutFs), reverse(Fs,RevFs), Out = [(RevFs,D,G)|OutFs].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% prepList(+[lambdaLi(W,Fs,Lambda,Depth,Group)],-[(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi])]])
%
% This function sorts the lambda-Li according to their depth and group, with the root at the beginning
%NB: Hier den Fehler in der Reihenfolge beheben (mal1000plus(5,plus10(4)))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prepList(Input,Output):-
  reverse(Input,Rev), % "sorted" by their depth, reverse the Input list, since the current order is leaf -> root
  groupLambda(Rev,[],Output). % sorted by their group, put the Lambda-Li in nested Lists, according to their groups, so that function(a(A1,A2),b(B),C) -> [function(X,Y,Z),[a(X,Y),[A1,A2]],[b(X),[B]],[C]]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% groupLambda(+[lambdaLi(W,Fs,Lambda,T,G)|[List_to_be_searched]],+[List_under_construction],-[(RootLambdaLi(W,Fs,Lambda,T,G),[InputLambdaLi])])
%
% This function sorts the lambda-Li according to their group, with the root at the beginning
% NB: Sprünge beachten
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

groupLambda([],Out,Out).
groupLambda([Li|RestLLI],[],Out):- groupLambda(RestLLI,[(Li,[])],Out).                              % first Item/last Item/Root-Li
groupLambda([lambdaLi(W,Fs,L,T,G)|RestLLI],[(lambdaLi(RW,RFs,RL,T,RG),LeafLis)|RestSorted],Out):-   % same depth
  compGroup(G,RG,before),%write("\n"), write(G),write(" before "), writeln(RG),
  groupLambda(RestLLI,[(lambdaLi(W,Fs,L,T,G),[]),(lambdaLi(RW,RFs,RL,T,RG),LeafLis)|RestSorted],Out); % New LI is to be sorted before the current LI
  %write("\n"), write(G),write(" after "), writeln(RG),
  groupLambda([lambdaLi(W,Fs,L,T,G)],RestSorted,NewSorted),                                            % New Li is to be sorted after current LI
  groupLambda(RestLLI,[(lambdaLi(RW,RFs,RL,T,RG),LeafLis)|NewSorted],Out).
groupLambda([lambdaLi(W,Fs,L,T,G)|RestLLI],[(lambdaLi(RW,RFs,RL,RT,RG),LeafLis)|RestSorted],Out):-  % different depth
  compGroup(G,RG,leaf),
  groupLambda([lambdaLi(W,Fs,L,T,G)],LeafLis,NewLeaf),
  groupLambda(RestLLI,[(lambdaLi(RW,RFs,RL,RT,RG),NewLeaf)|RestSorted],Out);                        % New LI is part of the group of current LI
  groupLambda([lambdaLi(W,Fs,L,T,G)],RestSorted,NewSorted),
  groupLambda(RestLLI,[(lambdaLi(RW,RFs,RL,RT,RG),LeafLis)|NewSorted],Out).                         % New LI is part of Later Group

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% compGroup(+[Grouping1],+[Grouping2],-Order)
%
% This function compares two groupings of LambdaLis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compGroup([],[],same).
compGroup(_,[],leaf).
compGroup([A|Rest1],[A|Rest2],Order):- !,compGroup(Rest1,Rest2,Order).
compGroup([A|_],[B|_],Order):-
  A > B,
  !, Order = after;    % A comes after B
  Order = before.          % B comes after A

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% fillYpsilon(+[(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi])],+[Features],-[(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi])]])
%
% This function inserts Ypsilon-Li, per group. The actual realisation of the algorithm
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fillYpsilon(PrepInput,Status,FilledList):-
  fillEpsilon(PrepInput,Status,PreCheckedList),
  (fDebugMode -> write("\nFilled List: "),writeln(PreCheckedList);true),
  checkDuringFillIn(PreCheckedList,HaltingFs),
  (HaltingFs = [([],_,_)],(fDebugMode -> writeln("\nAll is swell");true),
  FilledList = PreCheckedList;
  (fDebugMode -> write("\nA hole remains: "), writeln(HaltingFs);true),
  fillYpsilon(PreCheckedList,HaltingFs,FilledList)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% fillEpsilon(+[(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi])],+[Features],-[(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi])]])
%
% This function inserts Ypsilon-Li, per group. The actual realisation of the algorithm
% NB:
%     1- herausfinden, an welcher Stelle es stopt -> FlatStatus
%     2- dorthin gehen
%     3- nachschauen welche Feature-Kombination vorlegt
%     4- richtiges ypsilon-Li raussuchen
%     5- nur diese Stelle nochmal probieren
%       -> wenn es nicht geht -> 3
%       -> Abbruchbedingung für Schleife (Gut und Böse) bedenken
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fillEpsilon([],_,[]).
fillEpsilon([(lambdaLi(W,Fs,Lambda,Depth,Group),InputLambdaLis)|RestInDepth],[(FRoot,Depth,RGroup)|RestFs],Out):-        % Correct Depth, check if correct point NB: RGroup -> Group und zweiter Pfad weg, ist durch anderes Pattern vieleicht bereits abgedeckt
  compGroup(RGroup,Group,same),                                                                                           % Correct Point, check the kind of gap
  (fDebugMode -> write("\nFound correct Point: "), write(Group);true),
  inputNewLi((lambdaLi(W,Fs,Lambda,Depth,Group),InputLambdaLis),FRoot,NewGroup),
  groupLambda(NewGroup,[],NewSortedGroup),
  append(NewSortedGroup,RestInDepth,Out);
  (fDebugMode -> write("\nFound right group: "), writeln(Group);true),
  fillEpsilon(RestInDepth,[(FRoot,Depth,RGroup)|RestFs],DeeperOut),
  Out = [(lambdaLi(W,Fs,Lambda,Depth,Group),InputLambdaLis)|DeeperOut]. % not correct Group, check other groups
fillEpsilon([(lambdaLi(W,Fs,Lambda,Depth,Group),InputLambdaLis)|RestInDepth],[(FRoot,RDepth,RGroup)|RestFs],Out):-        % not Correct Depth, look at the deeper Lis NB: vieleicht überflüssig
  (fDebugMode -> write("\nFound wrong depth: "), writeln(Depth);true),
  compGroup(RGroup,Group,leaf),
  fillEpsilon(InputLambdaLis,[(FRoot,RDepth,RGroup)|RestFs],DeeperOut),
  Out = [(lambdaLi(W,Fs,Lambda,Depth,Group),DeeperOut)|RestInDepth];        % Gap is in this Group, but different Depth NB: vieleicht überflüssig
  fillEpsilon(RestInDepth,[(FRoot,RDepth,RGroup)|RestFs],DeeperOut),
  Out = [(lambdaLi(W,Fs,Lambda,Depth,Group),InputLambdaLis)|DeeperOut].     % Gap is not in this group, check the other ones

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% inputNewLi(+lambdaLi(W,Fs,Lambda,Depth,Group),+[InputLambdaLi],+[Halting_Features],-[LambdaLis]))
%
% This function goes to the feature gap and inserts at the correct location the Ypsilon-Li
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
inputNewLi(Lis, [], Lis).
inputNewLi((LLi,InputLis),[+_|_],Out):-
  (fDebugMode -> writeln("\nGap is between Function-Li and Scope-Li (Licensor)");true),                                         % Gap is between Function-Li to Scope-Li
  flattenLambdaLi([(LLi,InputLis)],FlattGroup),
  (fDebugMode -> write("\nFlattengroup: "), writeln(FlattGroup);true),
  extractFList(FlattGroup,[Head|InputLiFs]),splitHeadpos(Head,(PosHead,D,_)),
  SearchD is D + 1,
  inputYAfter(FlattGroup,PosHead,SearchD,InputLiFs,Out).
inputNewLi((LLi,InputLis),[=_|_],Out):-                                          % Gap is between Function-Li to Scope-Li
  (fDebugMode -> writeln("\nGap is between Function-Li and Scope-Li (Selektor)");true),
  flattenLambdaLi([(LLi,InputLis)],FlattGroup),
  (fDebugMode -> write("\nFlattengroup: "), writeln(FlattGroup);true),
  extractFList(FlattGroup,[Head|InputLiFs]),splitHeadpos(Head,(PosHead,D,_)),
  %write("\nextracted List: "), writeln([Head|InputLiFs]),
  SearchD is D + 1,
  inputYAfter(FlattGroup,PosHead,SearchD,InputLiFs,Out).
inputNewLi((LLi,InputLis),[ _|_],Out):-
  (fDebugMode -> writeln("\nGap is between startCategory and Function-Li");true),
  flattenLambdaLi([(LLi,InputLis)],FlattGroup),
  (fDebugMode -> write("\nFlattengroup: "), writeln(FlattGroup);true),
  extractFList(FlattGroup,[Head|_]),splitHeadneg(Head,(NegHead,_,_)),
  startCategory(A),
  inputYBefore(FlattGroup,[A],NegHead,Out).        % Gap is from Function-Li to Startcategory NB: später vieleicht auch einmal zum nächst höheren LI


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% inputYAfter(+[lambdaLi(W,Fs,Lambda,Depth,Group)|[InputLambdaLi]],+[Feature_Function_Li],+[InputFeatures_not_yet_their_turn],-[Rest_Features_Function_Li],-[InputLambdaLi]))
%
% This function goes to the feature gap and inserts between function-Li and the scope-Li the Ypsilon-Li
% NB:
%     - finde Ypsilin -Li, die zum letzten positiven Feature des Funktions-Li passen und alle negativen Features des Skopus-Li aufflösen
%     - versuche das gleiche mit dem nächsten Skopus-Li in der listening
%     - Falls es für eines der Skopus-Li nicht geht (es gibt keine Kombination an Y-Li die alle negativen-Features aufflösen und mit dem letzten aktuellen positiven Feature des Funktions-Li verbinden)
%       oder nach allen Skopus-Li noch positive Features beim Funktions-Li übrig bleibe, die nicht mit Y-Li alleine beseitigt werden können -> geh n Skopus-Li zurück und probier eine andere Kombination
% NB: mach es einmaliges patternmatching für alternative Pfade
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inputYAfter(Li,[],_,_,Li):- (fDebugMode -> writeln("\nNo hole to fill left");true).
inputYAfter([lambdaLi(W,Fs,Lambda,Depth,Group)|InputLambdaLi],[+F|RestFs],D,[],NewLis):- append(Group,[1],NewGroup), (fDebugMode -> writeln("\nTry Connect Lizensor and Void");true),connectLisLizCat([lambdaLi(W,Fs,Lambda,Depth,Group)|InputLambdaLi],[+F|RestFs],[],NewGroup,[],AlteredLi,NewFs,[]),(fDebugMode -> writeln("\nTry for the remaining Fs."),write("\nWith New LI-List: "), writeln(AlteredLi),write("\nRemaining Scope-Li: "),write([]),write("\nAnd new Fs: "),writeln(NewFs);true),inputYAfter(AlteredLi,NewFs,D,[],NewLis).
inputYAfter([lambdaLi(W,Fs,Lambda,Depth,Group)|InputLambdaLi],[=F|RestFs],D,[],NewLis):- append(Group,[1],NewGroup), (fDebugMode -> writeln("\nTry Connect Selector and Void");true),connectLisSelCat([lambdaLi(W,Fs,Lambda,Depth,Group)|InputLambdaLi],[=F|RestFs],[],NewGroup,[],AlteredLi,NewFs,[]),(fDebugMode -> writeln("\nTry for the remaining Fs."),write("\nWith New LI-List: "), writeln(AlteredLi),write("\nRemaining Scope-Li: "),write([]),write("\nAnd new Fs: "),writeln(NewFs);true),inputYAfter(AlteredLi,NewFs,D,[],NewLis).
inputYAfter(Li,[+F|RestFs],D,[([-G|LLiFs],D,Group)|RestLi],NewLis):- (fDebugMode -> writeln("\nTry Connect Lizensor and wrong Lizensee");true),connectLisLizLiz(Li,[+F|RestFs],[-G|LLiFs],Group,RestLi,AlteredLi,NewFs,AlteredRestLi),(fDebugMode -> writeln("\nTry for the remaining Fs."),write("\nWith New LI-List: "), writeln(AlteredLi),write("\nRemaining Scope-Li: "),write(AlteredRestLi),write("\nAnd new Fs: "),writeln(NewFs);true),inputYAfter(AlteredLi,NewFs,D,AlteredRestLi,NewLis).
inputYAfter(Li,[+F|RestFs],D,[([ G|LLiFs],D,Group)|RestLi],NewLis):- (fDebugMode -> writeln("\nTry Connect Lizensor and Category");true),connectLisLizCat(Li,[+F|RestFs],[ G|LLiFs],Group,RestLi,AlteredLi,NewFs,AlteredRestLi),(fDebugMode -> writeln("\nTry for the remaining Fs."),write("\nWith New LI-List: "), writeln(AlteredLi),write("\nRemaining Scope-Li: "),write(AlteredRestLi),write("\nAnd new Fs: "),writeln(NewFs);true),inputYAfter(AlteredLi,NewFs,D,AlteredRestLi,NewLis).
inputYAfter(Li,[=F|RestFs],D,[([-G|LLiFs],D,Group)|RestLi],NewLis):- (fDebugMode -> writeln("\nTry Connect Selektor and Lizensee");true),connectLisSelLiz(Li,[=F|RestFs],[-G|LLiFs],Group,RestLi,AlteredLi,NewFs,AlteredRestLi),(fDebugMode -> writeln("\nTry for the remaining Fs."),write("\nWith New LI-List: "), writeln(AlteredLi),write("\nRemaining Scope-Li: "),write(AlteredRestLi),write("\nAnd new Fs: "),writeln(NewFs);true),inputYAfter(AlteredLi,NewFs,D,AlteredRestLi,NewLis).
inputYAfter(Li,[=F|RestFs],D,[([ G|LLiFs],D,Group)|RestLi],NewLis):- (fDebugMode -> writeln("\nTry Connect Selektor and wrong Category");true),connectLisSelCat(Li,[=F|RestFs],[ G|LLiFs],Group,RestLi,AlteredLi,NewFs,AlteredRestLi),(fDebugMode -> writeln("\nTry for the remaining Fs."),write("\nWith New LI-List: "), writeln(AlteredLi),write("\nRemaining Scope-Li: "),write(AlteredRestLi),write("\nAnd new Fs: "),writeln(NewFs);true),inputYAfter(AlteredLi,NewFs,D,AlteredRestLi,NewLis).
inputYAfter(Li,Fs,D,[_|RestLi],NewLis):- (fDebugMode -> writeln("\nWrong Depth to connect.");true),inputYAfter(Li,Fs,D,RestLi,NewLis).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% inputYBefore(+(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi]),+Feature_Function_Li,+[InputFeatures_to_be_used],+[InputFeatures_not_yet_their_turn],-(lambdaLi(W,Fs,Lambda,Depth,Group),-[InputLambdaLi]))
%
% This function goes to the feature gap and inserts between the function-Li and the startcategory the Ypsilon-Li
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inputYBefore(Li,[],[],Li).
inputYBefore(Li,[F],[G|LLiFs],NewLis):-
  (fDebugMode -> write("\nTo Start: Remaining Features F:"), write([F]),write("S: "), writeln([G|LLiFs]);true),
  WY:: (Fs,[A,Out] >> (Out = A)),
  var(A),
  reverse(Fs,[F|RestYFs]),
  splitneg(RestYFs,[]),
  LiY = lambdaLi(WY,Fs,[A,Out] >> (Out = A),0,[0]),
  (fDebugMode -> write("\nTo start: Found Y-Li: "),writeln(LiY);true),
  matchYToScop(RestYFs,[],[G|LLiFs],NewFs,YList),
  (fDebugMode -> write("\nTo start: New Fs to work: "), writeln(NewFs);true),
  (fDebugMode -> write("\nTo start: Finished Y-Li list: "),writeln(YList);true),
  (fDebugMode -> write("\nTo start: Insert in: "),writeln(Li);true),
  (fDebugMode -> write("\nTo start: At Item: "),writeln([0]);true),
  insertY(Li,[LiY|YList],[0],NewLis).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% connectLisLizLiz(+[Lis_Group],+[Function_Features],+[Skope_Features],-[New_Li_Group],-[New_Function_Fs],-[NewTestLi])
%
% This function finds the correct Ypsilon-Lis to connect a Licensor and Licensee and inserts them
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connectLisLizLiz(Li,Fs,[],RLi,Li,Fs,RLi):- (fDebugMode -> write("\nLizLiz: No Scope-Fs left. F: "), writeln(Fs);true).
connectLisLizLiz(Li,[+F|RestFs],[-F|LLiFs],Group,RestLi,AlteredLi,NewFs,AlteredRestLi):-
  (fDebugMode -> write("\nLizLiz: same feature names. F: "), write([+F|RestFs]), write(" S: "), writeln([-F|LLiFs]);true),
  splitneg(LLiFs,NegLLiFs),
  matchYToScop(RestFs,[],NegLLiFs,NewFs,YList),
  (fDebugMode -> write("\nLizLiz: New Fs to work: "), writeln(NewFs);true),
  (fDebugMode -> write("\nLizLiz: Finished Y-Li list: "),writeln(YList);true),
  (fDebugMode -> write("\nLizLiz: Insert in: "),writeln(Li);true),
  (fDebugMode -> write("\nLizLiz: At Item: "),writeln(Group);true),
  insertY(Li,YList,Group,AlteredLi),
  (fDebugMode -> write("\nLizLiz: Correct Rest of LIs to check: "), writeln(RestLi);true),
  length(YList,YLength),
  (fDebugMode -> write("\nLizLiz: With additional Depth: "), writeln(YLength);true),
  (fDebugMode -> write("\nLizLiz: after group: "), writeln(Group);true),
  correctRestLi(RestLi,Group,YLength,AlteredRestLi),
  (fDebugMode -> write("\nLizLiz: New LIs to check (corrected): "), writeln(AlteredRestLi);true).
connectLisLizLiz(Li,[+F|RestFs],[-G|LLiFs],Group,RestLi,AlteredLi,NewFs,AlteredRestLi):-
  (fDebugMode -> write("\nLizLiz: different feature names. F: "), write([+F|RestFs]), write(" S: "), writeln([-G|LLiFs]);true),
  %matchFeatures(+F,-G,false),
  WY::(Fs,[A,Out] >> (Out = A)),
  var(A),
  %write("\nTry Li: "),writeln([]::([Fs],[A,Out] >> (Out = A))),
  reverse(Fs,[-F|RestYFs]),
  splitneg(RestYFs,NegYFs), matchFuncToY(RestFs,NegYFs,RemainFs,true),
  splitneg([-G|LLiFs],NegLLiFs),splitpos(RestYFs,PosYFs),
  LiY = lambdaLi(WY,Fs,[A,Out] >> (Out = A),0,[0]),
  (fDebugMode -> write("\nLizLiz: Found Y-Li: "),writeln(LiY);true),
  matchYToScop(RemainFs,PosYFs,NegLLiFs,NewFs,YList),
  (fDebugMode -> write("\nLizLiz: New Fs to work: "), writeln(NewFs);true),
  %reverse([-G|LLiFs],PointFs),
  (fDebugMode -> write("\nLizLiz: Finished Y-Li list: "),writeln([LiY|YList]);true),
  (fDebugMode -> write("\nLizLiz: Insert in: "),writeln(Li);true),
  (fDebugMode -> write("\nLizLiz: At Item: "),writeln(Group);true),
  insertY(Li,[LiY|YList],Group,AlteredLi),
  length([LiY|YList],YLength),
  correctRestLi(RestLi,Group,YLength,AlteredRestLi).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% connectLisLizCat(+[Lis_Group],+[Function_Features],+[Skope_Features],-[New_Li_Group],-[New_Function_Fs])
%
% This function finds the correct Ypsilon-Lis to connect a Licensor and Category and inserts them
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connectLisLizCat(Li,[+F|RestFs],ScopeLi,Group,RestLi,AlteredLi,NewFs,AlteredRestLi):-
  (fDebugMode -> write("\nLizCat: different feature types. F: "), write([+F|RestFs]), write(" S: "), writeln(ScopeLi);true),
  (WY::(Fs,[A,Out] >> (Out = A)), var(A),
  reverse(Fs,[-F|RestYFs]),
  splitneg(RestYFs,NegYFs), matchFuncToY(RestFs,NegYFs,RemainFs,true),
  splitneg(ScopeLi,NegLLiFs),splitpos(RestYFs,PosYFs),
  LiY = lambdaLi(WY,Fs,[A,Out] >> (Out = A),0,[0]);
  WY::(Fs,epsilon),
  reverse(Fs,[-F|RestYFs]),
  splitneg(RestYFs,NegYFs), matchFuncToY(RestFs,NegYFs,RemainFs,true),
  splitneg(ScopeLi,NegLLiFs),splitpos(RestYFs,PosYFs),
  LiY = lambdaLi(WY,Fs,epsilon,0,[0])),
  (fDebugMode -> write("\nLizCat: Found Y-Li: "),writeln(LiY);true),
  matchYToScop(RemainFs,PosYFs,NegLLiFs,NewFs,YList),
  append(YList,[LiY],NewYList),
  (fDebugMode -> write("\nLizCat: New Fs to work: "), writeln(NewFs);true),
  (fDebugMode -> write("\nLizCat: Finished Y-Li list: "),writeln(NewYList);true),
  (fDebugMode -> write("\nLizCat: Insert in: "),writeln(Li);true),
  (fDebugMode -> write("\nLizCat: At Item: "),writeln(Group);true),
  insertY(Li,NewYList,Group,AlteredLi),
  length(NewYList,YLength),
  correctRestLi(RestLi,Group,YLength,AlteredRestLi).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% connectLisSelLiz(+[Lis_Group],+[Function_Features],+[Skope_Features],-[New_Li_Group],-[New_Function_Fs])
%
% This function finds the correct Ypsilon-Lis to connect a Selector and Licensee and inserts them
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connectLisSelLiz(Li,[=F|RestFs],[-G|LLiFs],Group,RestLi,AlteredLi,NewFs,AlteredRestLi):-
  (fDebugMode -> write("\nSelLiz: different feature types. F: "), write([=F|RestFs]), write(" S: "), writeln([-G|LLiFs]);true),
  WY::(Fs,[A,Out] >> (Out = A)),var(A),
  reverse(Fs,[F|PosYFs]),
  LiY = lambdaLi(WY,Fs,[A,Out] >> (Out = A),0,[0]),
  (fDebugMode -> write("\nSelLiz: Found Y-Li: "),writeln(LiY);true),
  splitneg([-G|LLiFs],NegLLiFs),
  matchYToScop(RestFs,PosYFs,NegLLiFs,NewFs,YList),
  (fDebugMode -> write("\nSelLiz: New Fs to work: "), writeln(NewFs);true),
  %reverse([-G|LLiFs],PointFs),
  (fDebugMode -> write("\nSelLiz: Finished Y-Li list: "),writeln([LiY|YList]);true),
  (fDebugMode -> write("\nSelLiz: Insert in: "),writeln(Li);true),
  (fDebugMode -> write("\nSelLiz: At Item: "),writeln(Group);true),
  insertY(Li,[LiY|YList],Group,AlteredLi),
  length([LiY|YList],YLength),
  correctRestLi(RestLi,Group,YLength,AlteredRestLi).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% connectLisSelCat(+[Lis_Group],+[Function_Features],+[Skope_Features],-[New_Li_Group],-[New_Function_Fs])
%
% This function finds the correct Ypsilon-Lis to connect a Selector and Category and inserts them
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connectLisSelCat(Li,[=F|RestFs],[],Group,[],AlteredLi,NewFs,[]):-
  (fDebugMode -> write("\nSelCat: Void. F: "), write([=F|RestFs]), write(" S: "), writeln([]);true),
  WY::(Fs,epsilon),
  reverse(Fs,[ F|RestYFs]),
  splitneg(RestYFs,NegYFs), matchFuncToY(RestFs,NegYFs,RemainFs,true),
  splitpos(RestYFs,PosYFs),
  LiY = lambdaLi(WY,Fs,epsilon,0,[0]),
  (fDebugMode -> write("\nFound Y-Li: "),writeln(LiY);true),
  matchYToScop(RemainFs,PosYFs,[],NewFs,YList),
  (fDebugMode -> write("\nSelCat: New Fs to work: "), writeln(NewFs);true),
  (fDebugMode -> write("\nSelCat: Finished Y-Li list: "),writeln([LiY|YList]);true),
  (fDebugMode -> write("\nSelCat: Insert in: "),writeln(Li);true),
  (fDebugMode -> write("\nSelCat: At Item: "),writeln(Group);true),
  insertY(Li,[LiY|YList],Group,AlteredLi).
connectLisSelCat(Li,[=F|RestFs],[ F|LLiFs],Group,RestLi,AlteredLi,NewFs,AlteredRestLi):-
  (fDebugMode -> write("\nSelCat: same feature names. F: "), write([=F|RestFs]), write(" S: "), writeln([ F|LLiFs]);true),
  splitneg(LLiFs,NegLLiFs),
  matchYToScop(RestFs,[],NegLLiFs,NewFs,YList),
  (fDebugMode -> write("\nSelCat: New Fs to work: "), writeln(NewFs);true),
  (fDebugMode -> write("\nSelCat: Finished Y-Li list: "),writeln(YList);true),
  (fDebugMode -> write("\nSelCat: Insert in: "),writeln(Li);true),
  (fDebugMode -> write("\nSelCat: At Item: "),writeln(Group);true),
  insertY(Li,YList,Group,AlteredLi),
  length(YList,YLength),
  correctRestLi(RestLi,Group,YLength,AlteredRestLi).
connectLisSelCat(Li,[=F|RestFs],[ G|LLiFs],Group,RestLi,AlteredLi,NewFs,AlteredRestLi):-
  (fDebugMode -> write("\nSelCat: different feature names. F: "), write([=F|RestFs]), write(" S: "), writeln([ G|LLiFs]);true),
  matchFeatures(=F,G,false),
  WY::(Fs,[A,Out] >> (Out = A)),var(A),
  reverse(Fs,[ F|RestYFs]),
  splitneg(RestYFs,NegYFs), matchFuncToY(RestFs,NegYFs,RemainFs,true),
  splitneg([G|LLiFs],NegLLiFs),splitpos(RestYFs,PosYFs),
  LiY = lambdaLi(WY,Fs,[A,Out] >> (Out = A),0,[0]),
  (fDebugMode -> write("\nFound Y-Li: "),writeln(LiY);true),
  matchYToScop(RemainFs,PosYFs,NegLLiFs,NewFs,YList),
  (fDebugMode -> write("\nSelCat: New Fs to work: "), writeln(NewFs);true),
  (fDebugMode -> write("\nSelCat: Finished Y-Li list: "),writeln([LiY|YList]);true),
  (fDebugMode -> write("\nSelCat: Insert in: "),writeln(Li);true),
  (fDebugMode -> write("\nSelCat: At Item: "),writeln(Group);true),
  insertY(Li,[LiY|YList],Group,AlteredLi),
  length([LiY|YList],YLength),
  correctRestLi(RestLi,Group,YLength,AlteredRestLi).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% insertY(+[(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi])],+[lambdaLi(W,Fs,Lambda,Depth,Group)],-[(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi])])
% insertY(+[lambdaLi(W,Fs,Lambda,Depth,Group)],+[lambdaLi(W,Fs,Lambda,Depth,Group)],-[(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi])])
% This function takes Ypsilon-Lis and inserts them in the List of skope-Lis at the position of the correct Scope-Li. It furthermore calculates the new depths and groupings
% NB: Ordne die Skopus-Li der betreffenden Skopus-Li auch in die richtige GRuppe und Tiefe ein

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
insertY(Li,[],_,Li):- (fDebugMode -> writeln("\nReached bottom");true).
insertY([lambdaLi(Word,Features,Lambda,Depth,Group)|RestInput],[lambdaLi(WY,FsY,LY,_,_)|RestY],Group,Out):-
  (fDebugMode -> writeln("\nInsert Ypsilon");true),
  NewDepth is Depth + 1, append(Group,[1],NewGroup),
  insertY([lambdaLi(Word,Features,Lambda,NewDepth,NewGroup)],RestY,NewGroup,DeeperOut), % NB:an dieser Stelle nicht RestInput mitmachen, sonder RestInput seperat behandeln, um verbleibende Skopus-Li richtig einzuordnen
  length([lambdaLi(WY,FsY,LY,_,_)|RestY],YLength),
  (fDebugMode -> write("\nCorrect rest of Skope-Li: "), writeln(RestInput);true),
  (fDebugMode -> write("\nWith inserted Depth of "), writeln(YLength);true),
  correctRestLi(RestInput,Group,YLength,CorrectRest),
  append([lambdaLi(WY,FsY,LY,Depth,Group)|DeeperOut],CorrectRest,Out).
insertY([LambdaLi|RestInput],YLis,Group,Out):-
  (fDebugMode -> writeln("\nNot right point to insert ");true),
  insertY(RestInput,YLis,Group,DeeperOut),
  Out = [LambdaLi|DeeperOut].
  %write("\nNew List: "),writeln(Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% correctRestLi(+[lambdaLi],+Group,+Length,-[LambdaLi])
%
% This function alters the depth and group of the lambdaLis according to the inserted Y-Li
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
correctRestLi([],_,_,[]):- (fDebugMode -> writeln("\nNothing to correct left.");true).
correctRestLi([lambdaLi(Word,Features,Lambda,Depth,LiGroup)|RestLis],Group,Length,Out):-
  (compGroup(LiGroup,Group,leaf), (fDebugMode -> writeln("\nCorrect Leaf");true),
    NewDepth is Depth + Length, (fDebugMode -> write("\nNew Depth: "), writeln(NewDepth);true),
    makeNewGroup(LiGroup,Group,Length,NewGroup), (fDebugMode -> write("\nNew Group: "), writeln(NewGroup);true),
    correctRestLi(RestLis,Group,Length,DeeperOut),
    Out = [lambdaLi(Word,Features,Lambda,NewDepth,NewGroup)|DeeperOut];
    (fDebugMode -> writeln("\nNot a leaf");true),
    correctRestLi(RestLis,Group,Length,DeeperOut),
    Out = [lambdaLi(Word,Features,Lambda,Depth,LiGroup)|DeeperOut]).
correctRestLi([(Fs,Depth,LiGroup)|RestLis],Group,Length,Out):-
  (compGroup(LiGroup,Group,leaf), (fDebugMode -> writeln("\nCorrect Leaf");true),
    NewDepth is Depth + Length, (fDebugMode -> write("\nNew Depth: "), writeln(NewDepth);true),
    makeNewGroup(LiGroup,Group,Length,NewGroup), (fDebugMode -> write("\nNew Group: "), writeln(NewGroup);true),
    correctRestLi(RestLis,Group,Length,DeeperOut),
    Out = [(Fs,NewDepth,NewGroup)|DeeperOut];
    (fDebugMode -> writeln("\nNot a leaf");true),
    correctRestLi(RestLis,Group,Length,DeeperOut),
    Out = [(Fs,Depth,LiGroup)|DeeperOut]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% makeNewGroup(+Group,+Group,+Length,-Group)
%
% This function alters group of the lambdaLis according to the inserted Y-Li
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

makeNewGroup(LiGroup,[],0,LiGroup):- (fDebugMode -> writeln("\nNothing left to add.");true).
makeNewGroup(_,_,0,[]):- (fDebugMode -> writeln("\nSomething went wrong.");true).
makeNewGroup(LiGroup,[],Length,Out):-
  NewLength is Length - 1, NewLength >= 0, (fDebugMode -> writeln("\nAdd another 1 to the Group");true),
  (fDebugMode -> write("\nNew length: "), writeln(NewLength);true),
  makeNewGroup(LiGroup,[],NewLength,DeeperOut),
  Out = [1|DeeperOut].
makeNewGroup([A|LiGroup],[A|Group],Length,Out):-
  (fDebugMode -> writeln("\nStill finding gap between Groups");true),
  makeNewGroup(LiGroup,Group,Length,DeeperOut),
  Out = [A|DeeperOut].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% splitHeadpos(+([Positiv_Negative_Features],Depth,Group),-([Positive_FEatures],Depth,Group))
%
% This function removes the negative features from a feature list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

splitHeadpos(([],D,G),([],D,G)).
splitHeadpos(([=F|Fs],D,G),Out):-
  splitHeadpos((Fs,D,G), (OutF,D,G)), Out = ([=F|OutF],D,G).
splitHeadpos(([+F|Fs],D,G),Out):-
  splitHeadpos((Fs,D,G),(OutF,D,G)), Out = ([+F|OutF],D,G).
splitHeadpos(([-_|Fs],D,G),Out):-
  splitHeadpos((Fs,D,G),Out).
splitHeadpos(([_|Fs],D,G),Out):-
  splitHeadpos((Fs,D,G),Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% splitpos(+[Positiv_Negative_Features],-[Positive_FEatures])
%
% This function removes the negative features from a feature list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

splitpos([],[]).
splitpos([=F|Fs],Out):-
  splitpos(Fs, OutF), Out = [=F|OutF].
splitpos([+F|Fs],Out):-
  splitpos(Fs,OutF), Out = [+F|OutF].
splitpos([-_|Fs],Out):-
  splitpos(Fs,Out).
splitpos([_|Fs],Out):-
  splitpos(Fs,Out).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% splitHeadneg(+([Positiv_Negative_Features],Depth,Group),-([Negative_Features],Depth,Group))
%
% This function removes the positive features from a feature list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

splitHeadneg(([],D,G), ([],D,G)).
splitHeadneg(([=_|Fs],D,G),Out):-
  splitHeadneg((Fs,D,G),Out).
splitHeadneg(([+_|Fs],D,G),Out):-
  splitHeadneg((Fs,D,G),Out).
splitHeadneg(([-F|Fs],D,G),Out):-
  splitHeadneg((Fs,D,G),(OutF,D,G)), Out = ([-F|OutF],D,G).
splitHeadneg(([F|Fs],D,G),Out):-
  splitHeadneg((Fs,D,G),(OutF,D,G)), Out = ([ F|OutF],D,G).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% splitneg(+[Positiv_Negative_Features],-[Negative_Features])
%
% This function removes the positive features from a feature list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

splitneg([], []).
splitneg([=_|Fs],Out):-
  splitneg(Fs,Out).
splitneg([+_|Fs],Out):-
  splitneg(Fs,Out).
splitneg([-F|Fs],Out):-
  splitneg(Fs,OutF), Out = [-F|OutF].
splitneg([F|Fs],Out):-
  splitneg(Fs,OutF), Out = [ F|OutF].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% matchLis(+(lambdaLi,[ImputLis],-(lambdaLi,[InputLis]))
%
% This function matches the Root_Li and Input_Lis as far as possible backwards
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matchLis(lambdaLi(W,Fs,L,D,G),[],(lambdaLi(W,Fs,L,D,G),[])).
matchLis(lambdaLi(W,Fs,L,D,G),[(lambdaLi(LW,LFs,LL,LD,LG),LeafInput)|RestLis],Out):-
  checkDuringFillIn([(lambdaLi(W,Fs,L,D,G),[(lambdaLi(LW,LFs,LL,LD,LG),[])])],NewFs),
  (NewFs = [(NewRFsPos,D,G),(NewLFs,LD,LG)],splitneg((Fs,D,G),(NewRFsNeg,D,G)),append(NewRFsPos,NewRFsNeg,NewRFs);
   NewFs = [(NewRFs,D,G)],NewLFs = [];
   NewFs = [(NewLFs,LD,LG)], splitHeadneg((Fs,D,G),(NewRFs,D,G))),
  matchLis(lambdaLi(W,NewRFs,L,D,G),RestLis,(lambdaLi(W,DeeperFs,L,D,G),DeeperLis)),
  Out = (lambdaLi(W,DeeperFs,L,D,G),[(lambdaLi(LW,NewLFs,LL,LD,LG),LeafInput)|DeeperLis]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% matchFuncToY(+[Features_Function_Li]],+[Features_Y_Li],-[Remain_Fs],-Bool)
%
% This function matches the Function-Li to Y-Li, if possible
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matchFuncToY(_,[],[],true).
matchFuncToY([],_,[],false).
matchFuncToY([+F|RestFunc],[-F|RestY],NewFunc,Bool):-matchFuncToY(RestFunc,RestY,NewFunc,Bool).
matchFuncToY([+F|RestFunc],[-G|RestY],NewFunc,Bool):-matchFuncToY(RestFunc,[-G|RestY],DeeperFunc,Bool),NewFunc = [+F|DeeperFunc].
matchFuncToY([=F|RestFunc],[-_|_],[=F|RestFunc],false).
matchFuncToY([=F|RestFunc],[ F],RestFunc,true).
matchFuncToY([=F|RestFunc],[ G|RestY],NewFunc,Bool):-matchFuncToY(RestFunc,[ G|RestY],DeeperFunc,Bool),NewFunc = [=F|DeeperFunc].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% matchingYScope(+[Features_Function_Li]],+[Features_Y_Li],-Bool)
%
% This function matches the y-Features to scope-features, as far as possible
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matchingYScope([],RestNegLL,[],RestNegLL).
matchingYScope(RestPosY,[],RestPosY,[]).
matchingYScope([+F|RestY],[-F|RestSc],RestPosY,RestNegLL):- matchingYScope(RestY,RestSc,RestPosY,RestNegLL).
matchingYScope([=F|RestY],[ F|RestSc],RestPosY,RestNegLL):- matchingYScope(RestY,RestSc,RestPosY,RestNegLL).
matchingYScope([+F|RestY],[ C|RestSc],[+F|RestY],[ C|RestSc]).
matchingYScope([=F|RestY],[ C|RestSc],[=F|RestY],[ C|RestSc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% matchYToScop(+[Features_Function_Li],+[Features_Y_Li],+[Features_Scop_Li],-[Features_Function_Li_Rest],-[Y_Li])
%
% This function matches the Y-Li to Scope-Li, if possible, or finds necessary Y-Lis
% NB:
%     - Suche beginnt vom Funktions-Li
%     - überprüfen, wie viele negative Features vom Skopus-Li durch das Y-Li wegkommen (passen Selektor und Kategorie?)
%     - überprüfen, wie viele negative Features vom Skopus-Li dann durch die Rest-Features des Funktions-Li wegkommen (später vieleicht einmal)
%     - für restliche negative Features vom Skopus passende Y-Li finden
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matchYToScop(RestFs,[],[],RestFs,[]).
matchYToScop(RestFs,[],NegLLiFs,NewFs,YList):-
  matchingYScope(RestFs,NegLLiFs,RestPos,RestNegLL),
  (fDebugMode -> write("\nRemainig Features: F: "), write(RestPos), write(" Y: "),write([]),write(" S: "), writeln(RestNegLL);true),
  ( isEmpty(RestNegLL,true),(fDebugMode -> writeln("\nMatched completly.");true),
    NewFs = RestPos, YList =[];
    RestNegLL = [HeadF|RestSc],                           % some negative Features remain in the scope-Li
    WY::(Fs,[A,Out] >> (Out = A)),var(A),
    reverse(Fs,RFs),splitpos(RFs,[HeadFs|RestYFs]),matchFeatures(HeadF,HeadFs,true),
    %write("\nPotential next Li: "), writeln([]::(Fs,[A,Out] >> (Out = A))),
    splitneg(RFs,NegFs),append(RestPos,RestFs,NewFuncFs),matchFuncToY(NewFuncFs,NegFs,RemainFs,true),                                               %find matching Y-Li NB: hier könnte später mal eine Oder-Verknüpfung her, um entweder positive Function-Features, oder positive Y-Features zu matchen
    Li = lambdaLi(WY,Fs,[A,Out] >> (Out = A),0,[0]),
    (fDebugMode -> write("\nFound next Y-Li: "), writeln(Li);true),
    matchYToScop(RemainFs,RestYFs,RestSc,NewFs,MoreYs),                                                              % find more matching Y-Li if needed
    YList = [Li|MoreYs]).
matchYToScop(RestFs,PosYFs,NegLLiFs,NewFs,YList):-
  matchingYScope(PosYFs,NegLLiFs,RestPosY,RestNegLL),     % check how much the Y-Li matches with the scope-Li
  (fDebugMode -> write("\nRemainig Features: F: "), write(RestFs), write(" Y: "),write(RestPosY),write(" S: "), writeln(RestNegLL);true),
  ( isEmpty(RestNegLL,true), isEmpty(RestPosY,true), (fDebugMode -> writeln("\nMatched completly.");true),
    NewFs = RestFs, YList = [];                              % all negtive Features of the scope-Li are taken care of
    RestNegLL = [HeadF|RestSc],                           % some negative Features remain in the scope-Li
    WY::(Fs,[A,Out] >> (Out = A)),var(A),
    reverse(Fs,RFs),splitpos(RFs,[HeadFs|RestYFs]),matchFeatures(HeadF,HeadFs,true),
    %write("\nPotential next Li: "), writeln([]::(Fs,[A,Out] >> (Out = A))),
    splitneg(RFs,NegFs),append(RestPosY,RestFs,NewFuncFs),matchFuncToY(NewFuncFs,NegFs,RemainFs,true),               %find matching Y-Li NB: hier könnte später mal eine Oder-Verknüpfung her, um entweder positive Function-Features, oder positive Y-Features zu matchen
    Li = lambdaLi(WY,Fs,[A,Out] >> (Out = A),0,[0]),
    (fDebugMode -> write("\nFound next Y-Li: "), writeln(Li);true),
    matchYToScop(RemainFs,RestYFs,RestSc,NewFs,MoreYs),                                                              % find more matching Y-Li if needed
    YList = [Li|MoreYs]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% matchScopToY(+[Features_Function_Li],+[Features_Y_Li],+[Features_Scop_Li],-[Features_Function_Li_Rest],-[Y_Li])
%
% This function matches the Scop-Li to Y-Li and Function-Li, if possible, or finds necessary Y-Lis
% NB:
%     - Suche beginnt vom Skopus-Li
%     - überprüfen, wie viele negative Features vom Skopus-Li durch das Y-Li wegkommen (passen Selektor und Kategorie?)
%     - überprüfen, wie viele negative Features vom Skopus-Li dann durch die Rest-Features des Funktions-Li wegkommen (später vieleicht einmal)
%     - für restliche negative Features vom Skopus passende Y-Li finden
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matchScopToY(RestFs,[],[],RestFs,[]).
matchScopToY(RestFs,Fs,NegLLiFs,NewFs,YList):-
  splitpos(Fs,PosYFs),
  matchingYScope(PosYFs,NegLLiFs,[],RestNegLL),
  ( isEmpty(RestNegLL,true),
    Li = [],RestSc = [];                              % all negtive Features of the scope-Li are taken care of
    RestNegLL = [HeadF|RestSc],                           % some negative Features remain in the scope-Li
    WY::(YFs,[A,Out] >> (Out = A)),var(A), reverse(YFs,RFs),splitpos(RFs,[HeadFs|RestFs]),matchFeatures(HeadF,HeadFs,true),  %find matching Y-Li
    Li = lambdaLi(WY,YFs,[A,Out] >> (Out = A),0,[0])),
  splitneg(Fs,NegFs),removePairs(RestFs,NegFs,[],WPairsOldY),splitneg(RFs,NegRFs),append(NegRFs,WPairsOldY,NewNegFs),  % NB: hier noch Überlappung der beiden Y-Li ausmerzen
  ( matchFuncToY(RestFs,NewNegFs,NewFs,true),YList = [Li];
    matchScopToY(RestFs,NewNegFs,RestSc,NewFs,MoreYs),
    append(MoreYs,[Li],YList)).

removePairs([],NegY,[],NegY).
removePairs([+F|PosY],[-F|NegY],NewPos,NewNeg):- removePairs(PosY,NegY,NewPos,NewNeg).
removePairs([=F|PosY],[ F|NegY],NewPos,NewNeg):- removePairs(PosY,NegY,NewPos,NewNeg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flattenForWS(+[(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi])]],-[lambdaLi(W,Fs,Lambda,Depth,Group)])
%
% This function sorts the lambda-Li according to their depth and group, with the root at the beginning and flattens the List
% NB: muss auch Tiefe und Gruppierung neuberechnen, da, Ypsilon-Li erstmal direkt vor den jeweiligen Lambda-Li in der Liste eingefügt sind
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


flattenForWS([],[]).
flattenForWS(Input,Output):-
  flattenLambdaLi(Input,FlatLambdaLi),
  sortDepth(FlatLambdaLi,[],[],SortLi),
  reverse(SortLi,Output).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% sortDepth(+[lambdaLi(W,Fs,Lambda,Depth,Group)|[InputLambdaLi]],+[Stackfront],+[SemisortedList],-[lambdaLi(W,Fs,Lambda,Depth,Group)])
%
% This function sorts the LIs according to their depth
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sortDepth([],[],Li,Li).
sortDepth([lambdaLi(W1,Fs1,L1,D1,G1)|RestLi1],StackLi,[],Out):-
  append(StackLi,[lambdaLi(W1,Fs1,L1,D1,G1)],NewStack),
  sortDepth(RestLi1,[],NewStack,Out).
sortDepth([lambdaLi(W,Fs,L,D,G)|RestLi],[],[],Out):- sortDepth(RestLi,[],[lambdaLi(W,Fs,L,D,G)],Out).
sortDepth([lambdaLi(W1,Fs1,L1,D1,G1)|RestLi1], StackLi,[lambdaLi(W2,Fs2,L2,D2,G2)|RestLi2],Out):-
  ( D1 < D2,
    append(StackLi,[lambdaLi(W1,Fs1,L1,D1,G1),lambdaLi(W2,Fs2,L2,D2,G2)|RestLi2],NewStack),
    sortDepth(RestLi1,[],NewStack,Out);
    append(StackLi,[lambdaLi(W2,Fs2,L2,D2,G2)],NewStack),
    sortDepth([lambdaLi(W1,Fs1,L1,D1,G1)|RestLi1],NewStack,RestLi2,Out)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flattenLambdaLi(+[(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi])]],-[lambdaLi(W,Fs,Lambda,Depth,Group)])
%
% This function flattens the List
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flattenLambdaLi([],[]).
flattenLambdaLi([(LLi,[])],[LLi]).
flattenLambdaLi([(LLi,LeafLis)],Out):-
  flattenLambdaLi(LeafLis,FlatLis), Out = [LLi|FlatLis].
flattenLambdaLi([(LLi,LeafLis)|RestSorted],Out):-
  flattenLambdaLi(LeafLis,FlatLis), flattenLambdaLi(RestSorted,FlatSorted), append([LLi|FlatLis],FlatSorted,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% changeToLI(+[Lambda_Li],-[Li])
%
% This function changes the type of the Items from lambdaLi to li
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
changeToLI([],[]).
changeToLI([lambdaLi(W,Fs,Lambda,_,_)|LambdaLi],Li):- append([li(W,Fs,Lambda)],ULi,Li), changeToLI(LambdaLi,ULi).
    %!,isEmpty(FlatStatus,true),
    changeToLI(FlattenList,Output)              % transform to true LI
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkFinishStart(+[List_LambdaLi],-[(Halting_FeatureRoot,Depth,Group),(Halting_FeatureLeaf,Depth,Group)|RestFetures)])
%
% This Function checks if finished at the start
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkFinishStart([],[]).
checkFinishStart(Input, Output):-
  checkFinishFace(Input,Checked),
  (Checked = [([A],_,_)],             % check if only the startcategory remains
    startCategory(A),
    Output = [];  % yes, all is finished
    Output = Checked).              % no, continue with filling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkFinishFace(+[List_LambdaLi],-[(Halting_FeatureRoot,Depth,Group),(Halting_FeatureLeaf,Depth,Group)|RestFetures)])
%
% This Function is a interface for the checkFinish function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkFinishFace([],[]).
checkFinishFace(Input,Output):-
  reverse(Input,RevInput),
  extractFList(RevInput,[(Head,D,G)|Check]),
  %reverse(Head,H),
  splitpos(Head,H),
  splitneg(Head,NegH),

  checkFinish((H,D,G),Check,[],OutputDown),
  (OutputDown = [([],_,_)],
  Output = [(NegH,D,G)];
  startCategory(A),
  NegH = [A|_],
  append(OutputDown,[(NegH,D,G)],Output);
  append([(NegH,D,G)],OutputDown,Output)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkDuringFillIn(+[List_LambdaLi],-[(Halting_FeatureRoot,Depth,Group),(Halting_FeatureLeaf,Depth,Group)|RestFetures)])
%
% This Function is a interface for the checkFinish function during Fill-In
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkDuringFillIn([],[]).
checkDuringFillIn(Input,Output):-

  flattenLambdaLi(Input,FlattenList),
  extractFList(FlattenList,[Head|RestLis]),
  splitHeadpos(Head,PositiveHead),

  checkFinish(PositiveHead,RestLis,[],Output).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkFinish(+Feature_List_Root,+[Feature_List_Leafs],+[buffer_to_be_checked],-Halting_Feature)
%
% This Function checks, if the group is finished, meaning all Features of the root can be matched with the Leafs
% NB: der Check, ob zwei Feature-Listen verglichen werden dürfen muss exakter werden
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkFinish(([],D,G),[],[],[([],D,G)]).                                         % finished, all is matched
checkFinish(Head,[],[],[Head]).                                              % finished, no input left
checkFinish(Head,[],Non,[Head|Non]).                        % Not Finished, output is list of remaining Features
checkFinish(([],_,_),Rest,[],Out):- filterEmpty(Rest,[],[H|ToCheck]),checkFinish(H,ToCheck,[],Out).
checkFinish(([],_,_),Rest,Non,Out):- append(Non,Rest,List), filterEmpty(List,[],[H|ToCheck]), checkFinish(H,ToCheck,[],Out). % NB: Wann kommt das hier zum Einsatz?
checkFinish(([HeadR|RootFs],RD,RG),[([HeadL|LeafFs],LD,LG)|RestLeafs],NonFit,OutF):-
  matchFeatures(HeadR,HeadL,true),append(NonFit,[(LeafFs,LD,LG)|RestLeafs],LeafListUnfiltered),filterEmpty(LeafListUnfiltered,[],LeafList),checkFinish((RootFs,RD,RG),LeafList,[],OutF);  % Feature matched, check next in List
  append(NonFit,[([HeadL|LeafFs],LD,LG)],LeafList),checkFinish(([HeadR|RootFs],RD,RG),RestLeafs,LeafList,OutF). % Feature did not match, check next one and put the other in for later

checkLegal([],_,no).
checkLegal(_,[],yes).
checkLegal(_,[(_,_,[])|_],no).
checkLegal([A|Rest1],[(F,D,[A|Rest2])|RestNon],no):- checkLegal(Rest1,[(F,D,Rest2)|RestNon],no).
checkLegal(LG,[_|RestNon],yes) :- checkLegal(LG,RestNon,yes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% extractFList(+[lambdaLi(W,Fs,Lambda,Depth,Group)|[RestLambdaLi]],-[[FeatureList_Li_List]])
%
% This function excerts the feature lists from the List of Lis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extractFList([],[]).
extractFList([lambdaLi(_,Fs,_,D,G)|RestLambdaLi],Out):-
  extractFList(RestLambdaLi,OutFs), reverse(Fs,RevFs), Out = [(RevFs,D,G)|OutFs].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% prepList(+[lambdaLi(W,Fs,Lambda,Depth,Group)],-[(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi])]])
%
% This function sorts the lambda-Li according to their depth and group, with the root at the beginning
%NB: Hier den Fehler in der Reihenfolge beheben (mal1000plus(5,plus10(4)))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prepList(Input,Output):-
  reverse(Input,Rev), % "sorted" by their depth, reverse the Input list, since the current order is leaf -> root
  groupLambda(Rev,[],Output). % sorted by their group, put the Lambda-Li in nested Lists, according to their groups, so that function(a(A1,A2),b(B),C) -> [function(X,Y,Z),[a(X,Y),[A1,A2]],[b(X),[B]],[C]]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% groupLambda(+[lambdaLi(W,Fs,Lambda,T,G)|[List_to_be_searched]],+[List_under_construction],-[(RootLambdaLi(W,Fs,Lambda,T,G),[InputLambdaLi])])
%
% This function sorts the lambda-Li according to their group, with the root at the beginning
% NB: Sprünge beachten
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

groupLambda([],Out,Out).
groupLambda([Li|RestLLI],[],Out):- groupLambda(RestLLI,[(Li,[])],Out).                              % first Item/last Item/Root-Li
groupLambda([lambdaLi(W,Fs,L,T,G)|RestLLI],[(lambdaLi(RW,RFs,RL,T,RG),LeafLis)|RestSorted],Out):-   % same depth
  compGroup(G,RG,before),%write("\n"), write(G),write(" before "), writeln(RG),
  groupLambda(RestLLI,[(lambdaLi(W,Fs,L,T,G),[]),(lambdaLi(RW,RFs,RL,T,RG),LeafLis)|RestSorted],Out); % New LI is to be sorted before the current LI
  %write("\n"), write(G),write(" after "), writeln(RG),
  groupLambda([lambdaLi(W,Fs,L,T,G)],RestSorted,NewSorted),                                            % New Li is to be sorted after current LI
  groupLambda(RestLLI,[(lambdaLi(RW,RFs,RL,T,RG),LeafLis)|NewSorted],Out).
groupLambda([lambdaLi(W,Fs,L,T,G)|RestLLI],[(lambdaLi(RW,RFs,RL,RT,RG),LeafLis)|RestSorted],Out):-  % different depth
  compGroup(G,RG,leaf),
  groupLambda([lambdaLi(W,Fs,L,T,G)],LeafLis,NewLeaf),
  groupLambda(RestLLI,[(lambdaLi(RW,RFs,RL,RT,RG),NewLeaf)|RestSorted],Out);                        % New LI is part of the group of current LI
  groupLambda([lambdaLi(W,Fs,L,T,G)],RestSorted,NewSorted),
  groupLambda(RestLLI,[(lambdaLi(RW,RFs,RL,RT,RG),LeafLis)|NewSorted],Out).                         % New LI is part of Later Group

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% compGroup(+[Grouping1],+[Grouping2],-Order)
%
% This function compares two groupings of LambdaLis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compGroup([],[],same).
compGroup(_,[],leaf).
compGroup([A|Rest1],[A|Rest2],Order):- !,compGroup(Rest1,Rest2,Order).
compGroup([A|_],[B|_],Order):-
  A > B,
  !, Order = after;    % A comes after B
  Order = before.          % B comes after A

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% fillYpsilon(+[(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi])],+[Features],-[(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi])]])
%
% This function inserts Ypsilon-Li, per group. The actual realisation of the algorithm
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fillYpsilon(PrepInput,Status,FilledList):-
  fillEpsilon(PrepInput,Status,PreCheckedList),

  checkDuringFillIn(PreCheckedList,HaltingFs),
  (HaltingFs = [([],_,_)],
  FilledList = PreCheckedList;

  fillYpsilon(PreCheckedList,HaltingFs,FilledList)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% fillEpsilon(+[(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi])],+[Features],-[(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi])]])
%
% This function inserts Ypsilon-Li, per group. The actual realisation of the algorithm
% NB:
%     1- herausfinden, an welcher Stelle es stopt -> FlatStatus
%     2- dorthin gehen
%     3- nachschauen welche Feature-Kombination vorlegt
%     4- richtiges epsilon-Li raussuchen
%     5- nur diese Stelle nochmal probieren
%       -> wenn es nicht geht -> 3
%       -> Abbruchbedingung für Schleife (Gut und Böse) bedenken
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fillEpsilon([],_,[]).
fillEpsilon([(lambdaLi(W,Fs,Lambda,Depth,Group),InputLambdaLis)|RestInDepth],[(FRoot,Depth,RGroup)|RestFs],Out):-        % Correct Depth, check if correct point NB: RGroup -> Group und zweiter Pfad weg, ist durch anderes Pattern vieleicht bereits abgedeckt
  compGroup(RGroup,Group,same),                                                                                           % Correct Point, check the kind of gap

  inputNewLi((lambdaLi(W,Fs,Lambda,Depth,Group),InputLambdaLis),FRoot,NewGroup),
  groupLambda(NewGroup,[],NewSortedGroup),
  append(NewSortedGroup,RestInDepth,Out);

  fillEpsilon(RestInDepth,[(FRoot,Depth,RGroup)|RestFs],DeeperOut),
  Out = [(lambdaLi(W,Fs,Lambda,Depth,Group),InputLambdaLis)|DeeperOut]. % not correct Group, check other groups
fillEpsilon([(lambdaLi(W,Fs,Lambda,Depth,Group),InputLambdaLis)|RestInDepth],[(FRoot,RDepth,RGroup)|RestFs],Out):-        % not Correct Depth, look at the deeper Lis NB: vieleicht überflüssig

  compGroup(RGroup,Group,leaf),
  fillEpsilon(InputLambdaLis,[(FRoot,RDepth,RGroup)|RestFs],DeeperOut),
  Out = [(lambdaLi(W,Fs,Lambda,Depth,Group),DeeperOut)|RestInDepth];        % Gap is in this Group, but different Depth NB: vieleicht überflüssig
  fillEpsilon(RestInDepth,[(FRoot,RDepth,RGroup)|RestFs],DeeperOut),
  Out = [(lambdaLi(W,Fs,Lambda,Depth,Group),InputLambdaLis)|DeeperOut].     % Gap is not in this group, check the other ones

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% inputNewLi(+lambdaLi(W,Fs,Lambda,Depth,Group),+[InputLambdaLi],+[Halting_Features],-[LambdaLis]))
%
% This function goes to the feature gap and inserts at the correct location the Ypsilon-Li
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
inputNewLi(Lis, [], Lis).
inputNewLi((LLi,InputLis),[+_|_],Out):-
                                         % Gap is between Function-Li to Scope-Li
  flattenLambdaLi([(LLi,InputLis)],FlattGroup),

  extractFList(FlattGroup,[Head|InputLiFs]),splitHeadpos(Head,(PosHead,D,_)),
  SearchD is D + 1,
  inputYAfter(FlattGroup,PosHead,SearchD,InputLiFs,Out).
inputNewLi((LLi,InputLis),[=_|_],Out):-                                          % Gap is between Function-Li to Scope-Li

  flattenLambdaLi([(LLi,InputLis)],FlattGroup),

  extractFList(FlattGroup,[Head|InputLiFs]),splitHeadpos(Head,(PosHead,D,_)),
  %write("\nextracted List: "), writeln([Head|InputLiFs]),
  SearchD is D + 1,
  inputYAfter(FlattGroup,PosHead,SearchD,InputLiFs,Out).
inputNewLi((LLi,InputLis),[ _|_],Out):-

  flattenLambdaLi([(LLi,InputLis)],FlattGroup),

  extractFList(FlattGroup,[Head|_]),splitHeadneg(Head,(NegHead,_,_)),
  startCategory(A),
  inputYBefore(FlattGroup,[A],NegHead,Out).        % Gap is from Function-Li to Startcategory NB: später vieleicht auch einmal zum nächst höheren LI


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% inputYAfter(+[lambdaLi(W,Fs,Lambda,Depth,Group)|[InputLambdaLi]],+[Feature_Function_Li],+[InputFeatures_not_yet_their_turn],-[Rest_Features_Function_Li],-[InputLambdaLi]))
%
% This function goes to the feature gap and inserts between function-Li and the scope-Li the Ypsilon-Li
% NB:
%     - finde Ypsilin -Li, die zum letzten positiven Feature des Funktions-Li passen und alle negativen Features des Skopus-Li aufflösen
%     - versuche das gleiche mit dem nächsten Skopus-Li in der listening
%     - Falls es für eines der Skopus-Li nicht geht (es gibt keine Kombination an Y-Li die alle negativen-Features aufflösen und mit dem letzten aktuellen positiven Feature des Funktions-Li verbinden)
%       oder nach allen Skopus-Li noch positive Features beim Funktions-Li übrig bleibe, die nicht mit Y-Li alleine beseitigt werden können -> geh n Skopus-Li zurück und probier eine andere Kombination
% NB: mach es einmaliges patternmatching für alternative Pfade
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inputYAfter(Li,[],_,_,Li).
inputYAfter([lambdaLi(W,Fs,Lambda,Depth,Group)|InputLambdaLi],[+F|RestFs],D,[],NewLis):- append(Group,[1],NewGroup), connectLisLizCat([lambdaLi(W,Fs,Lambda,Depth,Group)|InputLambdaLi],[+F|RestFs],[],NewGroup,[],AlteredLi,NewFs,[]), inputYAfter(AlteredLi,NewFs,D,[],NewLis).
inputYAfter([lambdaLi(W,Fs,Lambda,Depth,Group)|InputLambdaLi],[=F|RestFs],D,[],NewLis):- append(Group,[1],NewGroup),connectLisSelCat([lambdaLi(W,Fs,Lambda,Depth,Group)|InputLambdaLi],[=F|RestFs],[],NewGroup,[],AlteredLi,NewFs,[]), inputYAfter(AlteredLi,NewFs,D,[],NewLis).
inputYAfter(Li,[+F|RestFs],D,[([-G|LLiFs],D,Group)|RestLi],NewLis):- connectLisLizLiz(Li,[+F|RestFs],[-G|LLiFs],Group,RestLi,AlteredLi,NewFs,AlteredRestLi), inputYAfter(AlteredLi,NewFs,D,AlteredRestLi,NewLis).
inputYAfter(Li,[+F|RestFs],D,[([ G|LLiFs],D,Group)|RestLi],NewLis):- connectLisLizCat(Li,[+F|RestFs],[ G|LLiFs],Group,RestLi,AlteredLi,NewFs,AlteredRestLi), inputYAfter(AlteredLi,NewFs,D,AlteredRestLi,NewLis).
inputYAfter(Li,[=F|RestFs],D,[([-G|LLiFs],D,Group)|RestLi],NewLis):- connectLisSelLiz(Li,[=F|RestFs],[-G|LLiFs],Group,RestLi,AlteredLi,NewFs,AlteredRestLi), inputYAfter(AlteredLi,NewFs,D,AlteredRestLi,NewLis).
inputYAfter(Li,[=F|RestFs],D,[([ G|LLiFs],D,Group)|RestLi],NewLis):- connectLisSelCat(Li,[=F|RestFs],[ G|LLiFs],Group,RestLi,AlteredLi,NewFs,AlteredRestLi), inputYAfter(AlteredLi,NewFs,D,AlteredRestLi,NewLis).
inputYAfter(Li,Fs,D,[_|RestLi],NewLis):- inputYAfter(Li,Fs,D,RestLi,NewLis).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% inputYBefore(+(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi]),+Feature_Function_Li,+[InputFeatures_to_be_used],+[InputFeatures_not_yet_their_turn],-(lambdaLi(W,Fs,Lambda,Depth,Group),-[InputLambdaLi]))
%
% This function goes to the feature gap and inserts between the function-Li and the startcategory the Ypsilon-Li
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inputYBefore(Li,[],[],Li).
inputYBefore(Li,[F],[G|LLiFs],NewLis):-

  WY:: (Fs,[A,Out] >> (Out = A)),
  var(A),
  reverse(Fs,[F|RestYFs]),
  splitneg(RestYFs,[]),
  LiY = lambdaLi(WY,Fs,[A,Out] >> (Out = A),0,[0]),

  matchYToScop(RestYFs,[],[G|LLiFs],_,YList),




  insertY(Li,[LiY|YList],[0],NewLis).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% connectLisLizLiz(+[Lis_Group],+[Function_Features],+[Skope_Features],-[New_Li_Group],-[New_Function_Fs],-[NewTestLi])
%
% This function finds the correct Ypsilon-Lis to connect a Licensor and Licensee and inserts them
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connectLisLizLiz(Li,Fs,[],RLi,Li,Fs,RLi).
connectLisLizLiz(Li,[+F|RestFs],[-F|LLiFs],Group,RestLi,AlteredLi,NewFs,AlteredRestLi):-

  splitneg(LLiFs,NegLLiFs),
  matchYToScop(RestFs,[],NegLLiFs,NewFs,YList),




  insertY(Li,YList,Group,AlteredLi),

  length(YList,YLength),


  correctRestLi(RestLi,Group,YLength,AlteredRestLi).
connectLisLizLiz(Li,[+F|RestFs],[-G|LLiFs],Group,RestLi,AlteredLi,NewFs,AlteredRestLi):-

  %matchFeatures(+F,-G,false),
  WY::(Fs,[A,Out] >> (Out = A)),
  var(A),
  %write("\nTry Li: "),writeln([]::([Fs],[A,Out] >> (Out = A))),
  reverse(Fs,[-F|RestYFs]),
  splitneg(RestYFs,NegYFs), matchFuncToY(RestFs,NegYFs,RemainFs,true),
  splitneg([-G|LLiFs],NegLLiFs),splitpos(RestYFs,PosYFs),
  LiY = lambdaLi(WY,Fs,[A,Out] >> (Out = A),0,[0]),

  matchYToScop(RemainFs,PosYFs,NegLLiFs,NewFs,YList),

  %reverse([-G|LLiFs],PointFs),



  insertY(Li,[LiY|YList],Group,AlteredLi),
  length([LiY|YList],YLength),
  correctRestLi(RestLi,Group,YLength,AlteredRestLi).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% connectLisLizCat(+[Lis_Group],+[Function_Features],+[Skope_Features],-[New_Li_Group],-[New_Function_Fs])
%
% This function finds the correct Ypsilon-Lis to connect a Licensor and Category and inserts them
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connectLisLizCat(Li,[+F|RestFs],ScopeLi,Group,RestLi,AlteredLi,NewFs,AlteredRestLi):-

  (WY::(Fs,[A,Out] >> (Out = A)), var(A),
  reverse(Fs,[-F|RestYFs]),
  splitneg(RestYFs,NegYFs), matchFuncToY(RestFs,NegYFs,RemainFs,true),
  splitneg(ScopeLi,NegLLiFs),splitpos(RestYFs,PosYFs),
  LiY = lambdaLi(WY,Fs,[A,Out] >> (Out = A),0,[0]);
  WY::(Fs,epsilon),
  reverse(Fs,[-F|RestYFs]),
  splitneg(RestYFs,NegYFs), matchFuncToY(RestFs,NegYFs,RemainFs,true),
  splitneg(ScopeLi,NegLLiFs),splitpos(RestYFs,PosYFs),
  LiY = lambdaLi(WY,Fs,epsilon,0,[0])),

  matchYToScop(RemainFs,PosYFs,NegLLiFs,NewFs,YList),
  append(YList,[LiY],NewYList),




  insertY(Li,NewYList,Group,AlteredLi),
  length(NewYList,YLength),
  correctRestLi(RestLi,Group,YLength,AlteredRestLi).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% connectLisSelLiz(+[Lis_Group],+[Function_Features],+[Skope_Features],-[New_Li_Group],-[New_Function_Fs])
%
% This function finds the correct Ypsilon-Lis to connect a Selector and Licensee and inserts them
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connectLisSelLiz(Li,[=F|RestFs],[-G|LLiFs],Group,RestLi,AlteredLi,NewFs,AlteredRestLi):-

  WY::(Fs,[A,Out] >> (Out = A)),var(A),
  reverse(Fs,[F|PosYFs]),
  LiY = lambdaLi(WY,Fs,[A,Out] >> (Out = A),0,[0]),

  splitneg([-G|LLiFs],NegLLiFs),
  matchYToScop(RestFs,PosYFs,NegLLiFs,NewFs,YList),

  %reverse([-G|LLiFs],PointFs),



  insertY(Li,[LiY|YList],Group,AlteredLi),
  length([LiY|YList],YLength),
  correctRestLi(RestLi,Group,YLength,AlteredRestLi).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% connectLisSelCat(+[Lis_Group],+[Function_Features],+[Skope_Features],-[New_Li_Group],-[New_Function_Fs])
%
% This function finds the correct Ypsilon-Lis to connect a Selector and Category and inserts them
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connectLisSelCat(Li,[=F|RestFs],[],Group,[],AlteredLi,NewFs,[]):-

  WY::(Fs,epsilon),
  reverse(Fs,[ F|RestYFs]),
  splitneg(RestYFs,NegYFs), matchFuncToY(RestFs,NegYFs,RemainFs,true),
  splitpos(RestYFs,PosYFs),
  LiY = lambdaLi(WY,Fs,epsilon,0,[0]),

  matchYToScop(RemainFs,PosYFs,[],NewFs,YList),


  insertY(Li,[LiY|YList],Group,AlteredLi).
connectLisSelCat(Li,[=F|RestFs],[ F|LLiFs],Group,RestLi,AlteredLi,NewFs,AlteredRestLi):-

  splitneg(LLiFs,NegLLiFs),
  matchYToScop(RestFs,[],NegLLiFs,NewFs,YList),

  insertY(Li,YList,Group,AlteredLi),
  length(YList,YLength),
  correctRestLi(RestLi,Group,YLength,AlteredRestLi).
connectLisSelCat(Li,[=F|RestFs],[ G|LLiFs],Group,RestLi,AlteredLi,NewFs,AlteredRestLi):-

  matchFeatures(=F,G,false),
  WY::(Fs,[A,Out] >> (Out = A)),var(A),
  reverse(Fs,[ F|RestYFs]),
  splitneg(RestYFs,NegYFs), matchFuncToY(RestFs,NegYFs,RemainFs,true),
  splitneg([G|LLiFs],NegLLiFs),splitpos(RestYFs,PosYFs),
  LiY = lambdaLi(WY,Fs,[A,Out] >> (Out = A),0,[0]),

  matchYToScop(RemainFs,PosYFs,NegLLiFs,NewFs,YList),

  insertY(Li,[LiY|YList],Group,AlteredLi),
  length([LiY|YList],YLength),
  correctRestLi(RestLi,Group,YLength,AlteredRestLi).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% insertY(+[(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi])],+[lambdaLi(W,Fs,Lambda,Depth,Group)],-[(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi])])
% insertY(+[lambdaLi(W,Fs,Lambda,Depth,Group)],+[lambdaLi(W,Fs,Lambda,Depth,Group)],-[(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi])])
% This function takes Ypsilon-Lis and inserts them in the List of skope-Lis at the position of the correct Scope-Li. It furthermore calculates the new depths and groupings
% NB: Ordne die Skopus-Li der betreffenden Skopus-Li auch in die richtige GRuppe und Tiefe ein

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
insertY(Li,[],_,Li).
insertY([lambdaLi(Word,Features,Lambda,Depth,Group)|RestInput],[lambdaLi(WY,FsY,LY,_,_)|RestY],Group,Out):-

  NewDepth is Depth + 1, append(Group,[1],NewGroup),
  insertY([lambdaLi(Word,Features,Lambda,NewDepth,NewGroup)],RestY,NewGroup,DeeperOut), % NB:an dieser Stelle nicht RestInput mitmachen, sonder RestInput seperat behandeln, um verbleibende Skopus-Li richtig einzuordnen
  length([lambdaLi(WY,FsY,LY,_,_)|RestY],YLength),

  correctRestLi(RestInput,Group,YLength,CorrectRest),
  append([lambdaLi(WY,FsY,LY,Depth,Group)|DeeperOut],CorrectRest,Out).
insertY([LambdaLi|RestInput],YLis,Group,Out):-

  insertY(RestInput,YLis,Group,DeeperOut),
  Out = [LambdaLi|DeeperOut].
  %write("\nNew List: "),writeln(Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% correctRestLi(+[lambdaLi],+Group,+Length,-[LambdaLi])
%
% This function alters the depth and group of the lambdaLis according to the inserted Y-Li
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
correctRestLi([],_,_,[]).
correctRestLi([lambdaLi(Word,Features,Lambda,Depth,LiGroup)|RestLis],Group,Length,Out):-
  (compGroup(LiGroup,Group,leaf),
    NewDepth is Depth + Length,
    makeNewGroup(LiGroup,Group,Length,NewGroup),
    correctRestLi(RestLis,Group,Length,DeeperOut),
    Out = [lambdaLi(Word,Features,Lambda,NewDepth,NewGroup)|DeeperOut];

    correctRestLi(RestLis,Group,Length,DeeperOut),
    Out = [lambdaLi(Word,Features,Lambda,Depth,LiGroup)|DeeperOut]).
correctRestLi([(Fs,Depth,LiGroup)|RestLis],Group,Length,Out):-
  (compGroup(LiGroup,Group,leaf),
    NewDepth is Depth + Length,
    makeNewGroup(LiGroup,Group,Length,NewGroup),
    correctRestLi(RestLis,Group,Length,DeeperOut),
    Out = [(Fs,NewDepth,NewGroup)|DeeperOut];

    correctRestLi(RestLis,Group,Length,DeeperOut),
    Out = [(Fs,Depth,LiGroup)|DeeperOut]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% makeNewGroup(+Group,+Group,+Length,-Group)
%
% This function alters group of the lambdaLis according to the inserted Y-Li
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

makeNewGroup(LiGroup,[],0,LiGroup).
makeNewGroup(_,_,0,[]).
makeNewGroup(LiGroup,[],Length,Out):-
  NewLength is Length - 1, NewLength >= 0,

  makeNewGroup(LiGroup,[],NewLength,DeeperOut),
  Out = [1|DeeperOut].
makeNewGroup([A|LiGroup],[A|Group],Length,Out):-

  makeNewGroup(LiGroup,Group,Length,DeeperOut),
  Out = [A|DeeperOut].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% splitHeadpos(+([Positiv_Negative_Features],Depth,Group),-([Positive_FEatures],Depth,Group))
%
% This function removes the negative features from a feature list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

splitHeadpos(([],D,G),([],D,G)).
splitHeadpos(([=F|Fs],D,G),Out):-
  splitHeadpos((Fs,D,G), (OutF,D,G)), Out = ([=F|OutF],D,G).
splitHeadpos(([+F|Fs],D,G),Out):-
  splitHeadpos((Fs,D,G),(OutF,D,G)), Out = ([+F|OutF],D,G).
splitHeadpos(([-_|Fs],D,G),Out):-
  splitHeadpos((Fs,D,G),Out).
splitHeadpos(([_|Fs],D,G),Out):-
  splitHeadpos((Fs,D,G),Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% splitpos(+[Positiv_Negative_Features],-[Positive_FEatures])
%
% This function removes the negative features from a feature list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

splitpos([],[]).
splitpos([=F|Fs],Out):-
  splitpos(Fs, OutF), Out = [=F|OutF].
splitpos([+F|Fs],Out):-
  splitpos(Fs,OutF), Out = [+F|OutF].
splitpos([-_|Fs],Out):-
  splitpos(Fs,Out).
splitpos([_|Fs],Out):-
  splitpos(Fs,Out).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% splitHeadneg(+([Positiv_Negative_Features],Depth,Group),-([Negative_Features],Depth,Group))
%
% This function removes the positive features from a feature list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

splitHeadneg(([],D,G), ([],D,G)).
splitHeadneg(([=_|Fs],D,G),Out):-
  splitHeadneg((Fs,D,G),Out).
splitHeadneg(([+_|Fs],D,G),Out):-
  splitHeadneg((Fs,D,G),Out).
splitHeadneg(([-F|Fs],D,G),Out):-
  splitHeadneg((Fs,D,G),(OutF,D,G)), Out = ([-F|OutF],D,G).
splitHeadneg(([F|Fs],D,G),Out):-
  splitHeadneg((Fs,D,G),(OutF,D,G)), Out = ([ F|OutF],D,G).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% splitneg(+[Positiv_Negative_Features],-[Negative_Features])
%
% This function removes the positive features from a feature list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

splitneg([], []).
splitneg([=_|Fs],Out):-
  splitneg(Fs,Out).
splitneg([+_|Fs],Out):-
  splitneg(Fs,Out).
splitneg([-F|Fs],Out):-
  splitneg(Fs,OutF), Out = [-F|OutF].
splitneg([F|Fs],Out):-
  splitneg(Fs,OutF), Out = [ F|OutF].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% matchLis(+(lambdaLi,[ImputLis],-(lambdaLi,[InputLis]))
%
% This function matches the Root_Li and Input_Lis as far as possible backwards
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matchLis(lambdaLi(W,Fs,L,D,G),[],(lambdaLi(W,Fs,L,D,G),[])).
matchLis(lambdaLi(W,Fs,L,D,G),[(lambdaLi(LW,LFs,LL,LD,LG),LeafInput)|RestLis],Out):-
  checkDuringFillIn([(lambdaLi(W,Fs,L,D,G),[(lambdaLi(LW,LFs,LL,LD,LG),[])])],NewFs),
  (NewFs = [(NewRFsPos,D,G),(NewLFs,LD,LG)],splitneg((Fs,D,G),(NewRFsNeg,D,G)),append(NewRFsPos,NewRFsNeg,NewRFs);
   NewFs = [(NewRFs,D,G)],NewLFs = [];
   NewFs = [(NewLFs,LD,LG)], splitHeadneg((Fs,D,G),(NewRFs,D,G))),
  matchLis(lambdaLi(W,NewRFs,L,D,G),RestLis,(lambdaLi(W,DeeperFs,L,D,G),DeeperLis)),
  Out = (lambdaLi(W,DeeperFs,L,D,G),[(lambdaLi(LW,NewLFs,LL,LD,LG),LeafInput)|DeeperLis]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% matchFuncToY(+[Features_Function_Li]],+[Features_Y_Li],-[Remain_Fs],-Bool)
%
% This function matches the Function-Li to Y-Li, if possible
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matchFuncToY(_,[],[],true).
matchFuncToY([],_,[],false).
matchFuncToY([+F|RestFunc],[-F|RestY],NewFunc,Bool):-matchFuncToY(RestFunc,RestY,NewFunc,Bool).
matchFuncToY([+F|RestFunc],[-G|RestY],NewFunc,Bool):-matchFuncToY(RestFunc,[-G|RestY],DeeperFunc,Bool),NewFunc = [+F|DeeperFunc].
matchFuncToY([=F|RestFunc],[-_|_],[=F|RestFunc],false).
matchFuncToY([=F|RestFunc],[ F],RestFunc,true).
matchFuncToY([=F|RestFunc],[ G|RestY],NewFunc,Bool):-matchFuncToY(RestFunc,[ G|RestY],DeeperFunc,Bool),NewFunc = [=F|DeeperFunc].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% matchingYScope(+[Features_Function_Li]],+[Features_Y_Li],-Bool)
%
% This function matches the y-Features to scope-features, as far as possible
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matchingYScope([],RestNegLL,[],RestNegLL).
matchingYScope(RestPosY,[],RestPosY,[]).
matchingYScope([+F|RestY],[-F|RestSc],RestPosY,RestNegLL):- matchingYScope(RestY,RestSc,RestPosY,RestNegLL).
matchingYScope([=F|RestY],[ F|RestSc],RestPosY,RestNegLL):- matchingYScope(RestY,RestSc,RestPosY,RestNegLL).
matchingYScope([+F|RestY],[ C|RestSc],[+F|RestY],[ C|RestSc]).
matchingYScope([=F|RestY],[ C|RestSc],[=F|RestY],[ C|RestSc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% matchYToScop(+[Features_Function_Li],+[Features_Y_Li],+[Features_Scop_Li],-[Features_Function_Li_Rest],-[Y_Li])
%
% This function matches the Y-Li to Scope-Li, if possible, or finds necessary Y-Lis
% NB:
%     - Suche beginnt vom Funktions-Li
%     - überprüfen, wie viele negative Features vom Skopus-Li durch das Y-Li wegkommen (passen Selektor und Kategorie?)
%     - überprüfen, wie viele negative Features vom Skopus-Li dann durch die Rest-Features des Funktions-Li wegkommen (später vieleicht einmal)
%     - für restliche negative Features vom Skopus passende Y-Li finden
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matchYToScop(RestFs,[],[],RestFs,[]).
matchYToScop(RestFs,[],NegLLiFs,NewFs,YList):-
  matchingYScope(RestFs,NegLLiFs,RestPos,RestNegLL),

  ( isEmpty(RestNegLL,true),
    NewFs = RestPos, YList =[];
    RestNegLL = [HeadF|RestSc],                           % some negative Features remain in the scope-Li
    WY::(Fs,[A,Out] >> (Out = A)),var(A),
    reverse(Fs,RFs),splitpos(RFs,[HeadFs|RestYFs]),matchFeatures(HeadF,HeadFs,true),
    %write("\nPotential next Li: "), writeln([]::(Fs,[A,Out] >> (Out = A))),
    splitneg(RFs,NegFs),append(RestPos,RestFs,NewFuncFs),matchFuncToY(NewFuncFs,NegFs,RemainFs,true),                                               %find matching Y-Li NB: hier könnte später mal eine Oder-Verknüpfung her, um entweder positive Function-Features, oder positive Y-Features zu matchen
    Li = lambdaLi(WY,Fs,[A,Out] >> (Out = A),0,[0]),

    matchYToScop(RemainFs,RestYFs,RestSc,NewFs,MoreYs),                                                              % find more matching Y-Li if needed
    YList = [Li|MoreYs]).
matchYToScop(RestFs,PosYFs,NegLLiFs,NewFs,YList):-
  matchingYScope(PosYFs,NegLLiFs,RestPosY,RestNegLL),     % check how much the Y-Li matches with the scope-Li

  ( isEmpty(RestNegLL,true), isEmpty(RestPosY,true),
    NewFs = RestFs, YList = [];                              % all negtive Features of the scope-Li are taken care of
    RestNegLL = [HeadF|RestSc],                           % some negative Features remain in the scope-Li
    WY::(Fs,[A,Out] >> (Out = A)),var(A),
    reverse(Fs,RFs),splitpos(RFs,[HeadFs|RestYFs]),matchFeatures(HeadF,HeadFs,true),
    %write("\nPotential next Li: "), writeln([]::(Fs,[A,Out] >> (Out = A))),
    splitneg(RFs,NegFs),append(RestPosY,RestFs,NewFuncFs),matchFuncToY(NewFuncFs,NegFs,RemainFs,true),                                               %find matching Y-Li NB: hier könnte später mal eine Oder-Verknüpfung her, um entweder positive Function-Features, oder positive Y-Features zu matchen
    Li = lambdaLi(WY,Fs,[A,Out] >> (Out = A),0,[0]),

    matchYToScop(RemainFs,RestYFs,RestSc,NewFs,MoreYs),                                                              % find more matching Y-Li if needed
    YList = [Li|MoreYs]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% matchScopToY(+[Features_Function_Li],+[Features_Y_Li],+[Features_Scop_Li],-[Features_Function_Li_Rest],-[Y_Li])
%
% This function matches the Scop-Li to Y-Li and Function-Li, if possible, or finds necessary Y-Lis
% NB:
%     - Suche beginnt vom Skopus-Li
%     - überprüfen, wie viele negative Features vom Skopus-Li durch das Y-Li wegkommen (passen Selektor und Kategorie?)
%     - überprüfen, wie viele negative Features vom Skopus-Li dann durch die Rest-Features des Funktions-Li wegkommen (später vieleicht einmal)
%     - für restliche negative Features vom Skopus passende Y-Li finden
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matchScopToY(RestFs,[],[],RestFs,[]).
matchScopToY(RestFs,Fs,NegLLiFs,NewFs,YList):-
  splitpos(Fs,PosYFs),
  matchingYScope(PosYFs,NegLLiFs,[],RestNegLL),
  ( isEmpty(RestNegLL,true),
    Li = [],RestSc = [];                              % all negtive Features of the scope-Li are taken care of
    RestNegLL = [HeadF|RestSc],                           % some negative Features remain in the scope-Li
    WY::(YFs,[A,Out] >> (Out = A)),var(A), reverse(YFs,RFs),splitpos(RFs,[HeadFs|RestFs]),matchFeatures(HeadF,HeadFs,true),  %find matching Y-Li
    Li = lambdaLi(WY,YFs,[A,Out] >> (Out = A),0,[0])),
  splitneg(Fs,NegFs),removePairs(RestFs,NegFs,[],WPairsOldY),splitneg(RFs,NegRFs),append(NegRFs,WPairsOldY,NewNegFs),  % NB: hier noch Überlappung der beiden Y-Li ausmerzen
  ( matchFuncToY(RestFs,NewNegFs,NewFs,true),YList = [Li];
    matchScopToY(RestFs,NewNegFs,RestSc,NewFs,MoreYs),
    append(MoreYs,[Li],YList)).

removePairs([],NegY,[],NegY).
removePairs([+F|PosY],[-F|NegY],NewPos,NewNeg):- removePairs(PosY,NegY,NewPos,NewNeg).
removePairs([=F|PosY],[ F|NegY],NewPos,NewNeg):- removePairs(PosY,NegY,NewPos,NewNeg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flattenForWS(+[(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi])]],-[lambdaLi(W,Fs,Lambda,Depth,Group)])
%
% This function sorts the lambda-Li according to their depth and group, with the root at the beginning and flattens the List
% NB: muss auch Tiefe und Gruppierung neuberechnen, da, Epsilon-Li erstmal direkt vor den jeweiligen Lambda-Li in der Liste eingefügt sind
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


flattenForWS([],[]).
flattenForWS(Input,Output):-
  flattenLambdaLi(Input,FlatLambdaLi),
  sortDepth(FlatLambdaLi,[],[],SortLi),
  reverse(SortLi,Output).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% sortDepth(+[lambdaLi(W,Fs,Lambda,Depth,Group)|[InputLambdaLi]],+[Stackfront],+[SemisortedList],-[lambdaLi(W,Fs,Lambda,Depth,Group)])
%
% This function sorts the LIs according to their depth
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sortDepth([],[],Li,Li).
sortDepth([lambdaLi(W1,Fs1,L1,D1,G1)|RestLi1],StackLi,[],Out):-
  append(StackLi,[lambdaLi(W1,Fs1,L1,D1,G1)],NewStack),
  sortDepth(RestLi1,[],NewStack,Out).
sortDepth([lambdaLi(W,Fs,L,D,G)|RestLi],[],[],Out):- sortDepth(RestLi,[],[lambdaLi(W,Fs,L,D,G)],Out).
sortDepth([lambdaLi(W1,Fs1,L1,D1,G1)|RestLi1], StackLi,[lambdaLi(W2,Fs2,L2,D2,G2)|RestLi2],Out):-
  ( D1 < D2,
    append(StackLi,[lambdaLi(W1,Fs1,L1,D1,G1),lambdaLi(W2,Fs2,L2,D2,G2)|RestLi2],NewStack),
    sortDepth(RestLi1,[],NewStack,Out);
    append(StackLi,[lambdaLi(W2,Fs2,L2,D2,G2)],NewStack),
    sortDepth([lambdaLi(W1,Fs1,L1,D1,G1)|RestLi1],NewStack,RestLi2,Out)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flattenLambdaLi(+[(lambdaLi(W,Fs,Lambda,Depth,Group),[InputLambdaLi])]],-[lambdaLi(W,Fs,Lambda,Depth,Group)])
%
% This function flattens the List
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flattenLambdaLi([],[]).
flattenLambdaLi([(LLi,[])],[LLi]).
flattenLambdaLi([(LLi,LeafLis)],Out):-
  flattenLambdaLi(LeafLis,FlatLis), Out = [LLi|FlatLis].
flattenLambdaLi([(LLi,LeafLis)|RestSorted],Out):-
  flattenLambdaLi(LeafLis,FlatLis), flattenLambdaLi(RestSorted,FlatSorted), append([LLi|FlatLis],FlatSorted,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% changeToLI(+[Lambda_Li],-[Li])
%
% This function changes the type of the Items from lambdaLi to li
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
changeToLI([],[]).
changeToLI([lambdaLi(W,Fs,Lambda,_,_)|LambdaLi],Li):- append([li(W,Fs,Lambda)],ULi,Li), changeToLI(LambdaLi,ULi).
