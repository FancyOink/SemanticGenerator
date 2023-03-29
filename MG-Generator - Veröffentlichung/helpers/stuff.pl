:- op(500, xfy, ::). % infix predicate for lexical items
:- op(500, fx, =). % for selection features

:- use_module(library(random)).
:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Makes a List of random numbers between 1 and 1 million
% randForStudi(+Number_of_random_numbers,-[List_of_random_numbers])
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
randForStudi(0,[]).
randForStudi(Num,List):-
  random(1,1000000,L),append(UList,[L],List),UNum is Num - 1,randForStudi(UNum,UList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% matchFeatures(+pos_Feature,+neg_Feature,-Bool)
%
% This function gives back a bool, depending if the feature match or not
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matchFeatures(+A,-A,true).
matchFeatures(-A,+A,true).
matchFeatures(=A, A,true).
matchFeatures( A, =A,true).
matchFeatures(_,_,false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% isEmpty(+[List],-Bool)
%
% This function gives back a bool, depending if the List is empty or not
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isEmpty([],true).
isEmpty(_,false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% filterEmpty(+[([List_of_List],D,G)],-[([Filtered_List_of_List],FD,DG)])
%
% This function filters empty list from a list of list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
filterEmpty([],List,List).
filterEmpty([([],_,_)],List,List).
filterEmpty([([],_,_)|Tail],Checked,List):- filterEmpty(Tail,Checked,List).
filterEmpty([Head|Tail],Checked, List):- append(Checked,[Head],UChecked), filterEmpty(Tail,UChecked,List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% dropSort(+N, +[List_before_drop], -[List_after_drop])
%
% This function drops the first N elements of a list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dropSort(0,Last,Last):-!.
dropSort(N,[_|Tail],Last):- N >0, N1 is N - 1, dropSort(N1,Tail,Last).
