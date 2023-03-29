% file: enrich.pl
% origin author : J. Kuhn
% origin date: Jan 2021
% purpose: enriches MG-lexicons with information to build a derivation tree
%   Tested with SWI-Prolog (threaded, 64 bits, version 7.6.4)
module(enrich,[enrich/0]).

% braucht es, damit ich die Einträge im Lexicon verändern darf
:- dynamic '::'/2.
% angeblich gibt es intern keinen Unterschied (derzeit) zwischen dynamic und static Fakten, außer das auf static Fakten kein assert/retract angewandt werden kann


% der Teil kann bei Einbindung nachher rausgenommen werden, da sonst doppelt
:- op(500, xfy, ::). % infix predicate for lexical items
:- op(500, fx, =). % for selection features

% uncomment one grammar
%:- ['../grammars/g1'].    % first simple example with wh-movement

%:- ['../grammars/g2'].    % copy language

%:- ['../grammars/g3'].    % example that uses lc1(merge2)

%:- ['../grammars/g6'].    % Bsp mit Maus/Käse

%:- ['../grammars/g7'].     % Bsp für  Feld-Nachricht

%:- ['../grammars/EnglishMG']. % Zahlwortgrammatik für Englisch 1 - 1 Mio

%:- ['../grammars/EnglishMG_v2']. % Zahlwortgrammatik für Englisch 1 - 1 Mio with changed featurelist

%:- ['../grammars/test']. % smaller testcase for EnglishMG
%:- ['../grammars/test_numbers']. % smaller testcase for EnglishMG
% NB: move the selection of the grammars to a config file
% For now, g2e parses can be specified, as shown in examples in each grammar file.
%:- ['../grammars/g2e'].   % copy-like language with empty categories
%:- ['../grammars/g8'].    % Bsp mit Maus/Käse zum Testen der enrich-Funktion

% NB: make "styles" an option for the enrichment (maybe in a config file)
% NB: verallgemeinern bezgl. einfügen der Featureliste
% top level function of the module, calls all Lexical items
enrich :-
    setof((W :: Fs), W^(W :: Fs), LexFeats),
    lexEnrich(LexFeats).

% saves the new enriched Lexical item and delets the old one
lexEnrich([]).
lexEnrich([Lex|LexS]) :-
    featEnrich(Lex, ELex),
    asserta(ELex),
    retract(Lex),
    lexEnrich(LexS).

% top level function to enrich the collected Lexical items
featEnrich((W :: Fs), (W :: EFs)) :- patternFeat(W, Fs, EFs,[]).

% pattern matching function to change all "simple" features to enriched ones
patternFeat(_, [], [], _).
patternFeat(W,[=F|Fs],EFs, ListFeat) :-
  Term =.. [F,X],
  dif(X,[]), dif(X,o),
  [=Term|RestEFs] = EFs,  % NB: Weg finden die Variable X hier einzufügen, ohne sie während des Anreichernds zu instanziieren
  append(ListFeat,[X],ListFeatS),
  patternFeat(W, Fs,RestEFs,ListFeatS).
patternFeat(W,[+F|Fs],EFs, ListFeat) :-
  [+F|RestEFs] = EFs,
  append(ListFeat, [o],ListFeatS),
  patternFeat(W, Fs,RestEFs,ListFeatS).
patternFeat(W,[-F|Fs], EFs, ListFeat) :-
  [-F|RestFeat] = EFs,
  append(ListFeat, [o],ListFeatS),
  patternFeat(W,Fs,RestFeat,ListFeatS).
patternFeat(W,[F|Fs], EFs, ListFeat) :-
  length(ListFeat,_),  % only here, because otherwise the second feature is not correctly registered. For some reason
  categoryFeat(W,F,ListFeat,Cat),
  [Cat|RestFeat] = EFs,
  patternFeat(W,Fs,RestFeat,[]).
patternFeat(_,_,_,_) :- nl, writeln('Feature list not legal. Maybe the order of the features is wrong. Please use: Selectors,Licensors, Category, Licensees.'). % Currently only happens, if a Selector or Licensor comes AFTER the category. Does NOT catch: multiple Categorys, License befor Category


% enriches the category-feature according to the features that came before

categoryFeat([],F,[],Cat) :-
    ( atomic(F) -> Cat =.. [F,e/[]]
    ; F = Cat
    ).
categoryFeat([W],F,[],Cat) :-
    ( atomic(F) ->Cat =.. [F,W/[]]
    ; F = Cat
    ).
categoryFeat([],F,[o|Rest],Cat) :-
    ( atomic(F) -> Term =.. [F,'o'/[e/[]]], categoryFeat([],Term,Rest,Cat) % the category-feature is not yet enriched at all, so the first transformation with move has to happen
    ; (arg(_,F,FArg), functor(F, FName, _), Term =.. [FName,'o'/[FArg]], categoryFeat([],Term,Rest,Cat)) % the categiry-feature was already enriched, so a new term has to be build out of the previous enriched category-feature plus the new move
    ).
categoryFeat([W],F,[o|Rest],Cat) :-
    ( atomic(F) -> Term =.. [F,'o'/[W/[]]], categoryFeat([W],Term,Rest,Cat) % the category-feature is not yet enriched at all, so the first transformation with move has to happen
    ; (arg(_,F,FArg), functor(F, FName, _), Term =.. [FName,'o'/[FArg]], categoryFeat([W],Term,Rest,Cat)) % the categiry-feature was already enriched, so a new term has to be build out of the previous enriched category-feature plus the new move
    ).
categoryFeat([],F,[X|Rest],Cat) :-
    ( atomic(F) ->  Term =.. [F,'*'/[e/[],X]],  categoryFeat([],Term,Rest,Cat) % the category-feature is not yet enriched at all, so the first transformation with merge has to happen
    ; (arg(_,F,FArg),functor(F,FName, _), Term =.. [FName,'*'/[FArg,X]], categoryFeat([],Term,Rest,Cat)) % the categiry-feature was already enriched, so a new term has to be build out of the previous enriched category-feature plus the new merge
    ).
categoryFeat([W],F,[X|Rest],Cat) :-
    ( atomic(F) ->  Term =.. [F,'*'/[W/[],X]],  categoryFeat([W],Term,Rest,Cat) % the category-feature is not yet enriched at all, so the first transformation with merge has to happen
    ; (arg(_,F,FArg),functor(F,FName, _), Term =.. [FName,'*'/[FArg,X]], categoryFeat([W],Term,Rest,Cat)) % the categiry-feature was already enriched, so a new term has to be build out of the previous enriched category-feature plus the new merge
    ).

% setof((W :: Fs), W^(W::Fs),LexFeat),[R|Rs]=LexFeat,(Wf :: Ff) = R,asserta(Wf :: [Ff|d]),retract(R).
