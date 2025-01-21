% file: main.pl
% origin author : J. Kuhn
% origin date: June 2021
% purpose: top executable for MG Generator

:- ['load'].

debugMode.
debugMode:- false.

generate(Expr,Sentance,Lambda,Tree):-
  lambdaSelectFkt(Expr,List),
  epsilonSelectFkt(List,Filled),
  generateExp(Filled,Tree),
  spellCheck(Expr,Tree,Sentance,Lambda),
  tree_painter(Tree).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Examples:
%
% Maus frisst Käse und Katze frisst Maus
% makeSentance([li(['Maus'],[d]),li(['Katze'],[d]),li([frisst],[=d,=d,v]),li(['Kaese'],[d]),li(['Maus'],[d]),li([frisst],[=d,=d,v]),li([und],[=c,=c,c]),li([],[=v,c]),li([],[=v,c])],Sentance, Tree).
%
% Maus frisst Käse und Katze frisst Maus mit Lambda
% makeLambdaSentance([li(['Maus'],[d],'Maus'),li(['Katze'],[d],'Katze'),li([frisst],[=d,=d,v],[A,B,Out]>>(Out = frisst(B,A))),li([],[=v,c],epsilon),li(['Kaese'],[d],'Kaese'),li(['Maus'],[d],'Maus'),li([frisst],[=d,=d,v],[A,B,Out]>>(Out= frisst(B,A))),li([],[=v,c],epsilon),li([und],[=c,=c,c],[B,A,Out]>>(Out = und(A,B)))],Sentance,Lambda,Tree).
%
% Hund weis nix und Maus frisst Käse und Katze frisst Maus mit Lambda
% makeLambdaSentance([li(['Maus'],[d],'Maus'),li(['Katze'],[d],'Katze'),li([frisst],[=d,=d,v],[A,B,Out]>>(Out = frisst(B,A))),li([],[=v,c],epsilon),li(['Kaese'],[d],'Kaese'),li(['Maus'],[d],'Maus'),li([frisst],[=d,=d,v],[A,B,Out]>>(Out= frisst(B,A))),li([],[=v,c],epsilon),li([und],[=c,=c,c],[B,A,Out]>>(Out = und(A,B))),li(['Hund'],[d,-w],'Hund'),li(['weis nix'],[=d,+w,c],[X,Out]>>(Out = weis(X,nix))),li([und],[=c,=c,c],[B,A,Out]>>(Out = und(A,B)))],Sentance,Lambda,Tree).
%
%  e Maus frisst Kaese und e Katze frisst Maus
% makeSentance([li(['Maus'],[d]),li(['Katze'],[d]),li([frisst],[=d,=d,v]),li(['Kaese'],[d]),li(['Maus'],[d]),li([frisst],[=d,=d,v]),li([und],[=c,=c,c]),li([e],[=v,c]),li([e],[=v,c])],Sentance, Tree).
%
% Maus frisst Käse mit Lambda
% makeLambdaSentance([li(['Kaese'],[d],'Kaese'),li(['Maus'],[d],'Maus'),li([frisst],[=d,=d,v],[A,B,Out]>>(Out = frisst(B,A)))],Sentance,Lambda,Tree).
%
% Katze frisst Maus
% makeSentance([li(['Maus'],[d]),li(['Katze'],[d]),li([frisst],[=d,=d,v])],Sentance, Tree).
%
% V1: achtunddreißig
% makeSentance([li([],[=c1,+ssi,+zeh,+un,c1,-ssi]),li(['ßig'],[=c1,+ssi,cundZIG]),li([drei],[c1,-ssi,-zeh,-un]),li([],[=c1,+zi,c1]),li([acht],[c1,-zi,-zeh,-un]),li([],[=c1,+zeh,c1]),li([],[=c2,+taus_,c3,-taus]),li([],[=c3,+taus,c3]),li([],[=c3,c4]),li([und],[=cundZIG,=c1,+un,c2,-taus_])],Sentance, Tree).
%
% V1: sieben mit Lambda
% makeLambdaSentance([li([sieben],[c1,-un],7),li([],[=c1,+un,c1],epsilon),li([],[=c1,c2],epsilon),li([],[=c2,c3],epsilon),li([],[=c3,c4],epsilon)],Sentance,Lambda,Out).
%
% V1:  achtunddreißig für latex
% makeSentance([li([],[=c1,+ssi,+zeh,+un,c1,-ssi]),li(['ssig'],[=c1,+ssi,cundZIG]),li([drei],[c1,-ssi,-zeh,-un]),li([],[=c1,+zi,c1]),li([acht],[c1,-zi,-zeh,-un]),li([],[=c1,+zeh,c1]),li([],[=c2,+taus_,c3,-taus]),li([],[=c3,+taus,c3]),li([],[=c3,c4]),li([und],[=cundZIG,=c1,+un,c2,-taus_])],Sentance, Tree).
%
% V1: achtunddreißig mit Lambda
% makeLambdaSentance([li([],[=c1,+ssi,+zeh,+un,c1,-ssi],epsilon),li(['ssig'],[=c1,+ssi,cundZIG],[A,Out]>>(Out is 0 * A + 30)),li([drei],[c1,-ssi,-zeh,-un],3),li([],[=c1,+zi,c1],epsilon),li([acht],[c1,-zi,-zeh,-un],8),li([],[=c1,+zeh,c1],epsilon),li([],[=c2,+taus_,c3,-taus],epsilon),li([],[=c3,+taus,c3], epsilon),li([],[=c3,c4],epsilon),li([und],[=cundZIG,=c1,+un,c2,-taus_],[A,B,Out]>>(Out is (A + B)))], Sentance, Lambda,Tree).
%
% V1:fünftausendvierzehn
% makeSentance([li([],[=c1,+zi,c1]),li([vier],[c1,-zi,-zeh,-un]),li([],[=c1,+zeh,+un,c1,-zeh]),li([zehn],[=c1,+zeh,c2,-taus_]),li([],[=c2,+taus_,c3,-taus]),li([],[=c3,+taus,c3]),li([],[=c1,+zi,c1]),li(['fuenf'],[c1,-zi,-zeh,-un]),li([],[=c1,+zeh,c1]),li([],[=c1,+un,c2,-taus_]),li([],[=c2,+taus_,c3,-taus]),li([tausend],[=c3,=c3,+taus,c4])],Sentance, Tree).
%
% V1:fünftausendvierzehn mit Lambda
% makeLambdaSentance([li([],[=c1,+zi,c1],epsilon),li([vier],[c1,-zi,-zeh,-un],4),li([],[=c1,+zeh,+un,c1,-zeh],epsilon),li([zehn],[=c1,+zeh,c2,-taus_],[A,Out]>>plus(A,10,Out)),li([],[=c2,+taus_,c3,-taus],epsilon),li([],[=c3,+taus,c3],epsilon),li([],[=c1,+zi,c1],epsilon),li(['fuenf'],[c1,-zi,-zeh,-un],5),li([],[=c1,+zeh,c1],epsilon),li([],[=c1,+un,c2,-taus_],epsilon),li([],[=c2,+taus_,c3,-taus],epsilon),li([tausend],[=c3,=c3,+taus,c4],[A,B,Out]>>(Out is (B * 1000 + A)))],Sentance, Lambda, Tree).
%
% makeLambdaSentance([li([],[=c1,+zi,c1],epsilon),li([vier],[c1,-zi,-zeh,-un],4),li([],[=c1,+zeh,+un,c1,-zeh],epsilon),li([zehn],[=c1,+zeh,c2,-taus_],[A,Out]>>(Out = plus10(A))),li([],[=c2,+taus_,c3,-taus],epsilon),li([],[=c3,+taus,c3],epsilon),li([],[=c1,+zi,c1],epsilon),li(['fuenf'],[c1,-zi,-zeh,-un],5),li([],[=c1,+zeh,c1],epsilon),li([],[=c1,+un,c2,-taus_],epsilon),li([],[=c2,+taus_,c3,-taus],epsilon),li([tausend],[=c3,=c3,+taus,c4],[A,B,Out]>>(Out = mal1000plus(B,A)))],Sentance, Lambda, Tree).
% makeLambdaSentance([li([vier],[c1,-zi,-zeh,-un],4),li([],[=c1,+zi,c1],epsilon),li([],[=c1,+zeh,+un,c1,-zeh],epsilon),li([zehn],[=c1,+zeh,c2,-taus_],[A,Out]>>(Out = plus10(A))),li([],[=c2,+taus_,c3,-taus],epsilon),li([],[=c3,+taus,c3],epsilon),li(['fuenf'],[c1,-zi,-zeh,-un],5),li([],[=c1,+zi,c1],epsilon),li([],[=c1,+zeh,c1],epsilon),li([],[=c1,+un,c2,-taus_],epsilon),li([],[=c2,+taus_,c3,-taus],epsilon),li([tausend],[=c3,=c3,+taus,c4],[A,B,Out]>>(Out = mal1000plus(B,A)))],Sentance, Lambda, Tree).
%
% V1: 85329 mit Lambda - Bsp. Einordnung Epsilon nach Lambda-Li (geht nicht)
% makeLambdaSentance([li([neun],[c1, -zi, -zeh, -un],9),li([],[=c1, +zi, c1],epsilon),li([],[=c1, +zeh, c1],epsilon),li([acht],[c1, -zi, -zeh, -un],8),li([],[=c1, +zi, +zeh, +un, c1, -zi],epsilon),li([undzwanzig],[=c1, +un, c2, -taus_],[A,Out] >> (Out = plus20(A))),li([],[=c2, +taus_, c2],epsilon),li([drei],[c1, -ssi, -zeh, -un],3),li([],[=c1, +ssi, c1],epsilon),li([],[=c1, +zeh, c1],epsilon),li([zig],[=c1, +zi, cundZIG],[A,Out]>> (Out = mal10A(A))),li([fuenf],[c1, -zi, -zeh, -un],5),li([],[=c1, +zi, c1],epsilon),li([],[=c1, +zeh, c1],epsilon),li([hundert],[=c2, =c1, +un, c3, -taus],[A,B,Out] >> (Out = mal100plus(B,A))),li([],[=c3, +taus, c3],epsilon),li([und],[ =cundZIG, =c1, +un, c2, -taus_],[A,B,Out] >> (Out = und(A,B))),li([],[=c2, +taus_, c3, -taus],epsilon),li([tausend],[=c3, =c3, +taus, c4],[A,B,Out] >> (Out = mal1000plus(B,A)))],Sentance,Lambda,Tree).
%
% V1: 85329 mit Lambda - Bsp. Sortierung aller LI nach Tiefe und Gruppierung
% makeLambdaSentance([li([neun],[c1, -zi, -zeh, -un],9),li([],[=c1, +zi, c1],epsilon),li([],[=c1, +zeh, c1],epsilon),li([drei],[c1, -ssi, -zeh, -un],3),li([fuenf],[c1, -zi, -zeh, -un],5),li([acht],[c1, -zi, -zeh, -un],8),li([undzwanzig],[=c1, +un, c2, -taus_],[A,Out] >> (Out = plus20(A))),li([],[=c1, +ssi, c1],epsilon),li([],[=c1, +zi, c1],epsilon),li([],[=c1, +zi, +zeh, +un, c1, -zi],epsilon),li([],[=c2, +taus_, c2],epsilon),li([],[=c1, +zeh, c1],epsilon),li([],[=c1, +zeh, c1],epsilon),li([zig],[=c1, +zi, cundZIG],[A,Out]>> (Out = mal10A(A))),li([hundert],[=c2, =c1, +un, c3, -taus],[A,B,Out] >> (Out = mal100plus(B,A))),li([und],[ =cundZIG, =c1, +un, c2, -taus_],[A,B,Out] >> (Out = und(A,B))),li([],[=c3, +taus, c3],epsilon),li([],[=c2, +taus_, c3, -taus],epsilon),li([tausend],[=c3, =c3, +taus, c4],[A,B,Out] >> (Out = mal1000plus(B,A)))], Sentance,Lambda,Tree).
%
% V1: 486649 mit Lambda
% makeLambdaSentance([li([vier],[c1, -zi, -zeh, -un],4),li([neun],[c1, -zi, -zeh, -un],9),li([],[=c1, +zi, +zeh, +un, c1, -zi],epsilon),li([],[=c1, +zi, c1],epsilon),li([acht],[c1, -zi, -zeh, -un],8),li([zig],[=c1, +zi, cundZIG],[A,Out]>> (Out = mal10A(A))),li([],[=c1, +zeh, c1],epsilon),li([],[=c1, +zi, +zeh, +un, c1, -zi],epsilon),li([und],[ =cundZIG, =c1, +un, c2, -taus_],[A,B,Out] >> (Out = und(B,A))),li([zig],[=c1, +zi, cundZIG],[A,Out]>> (Out = mal10A(A))),li([sechs],[c1, -un],6),li([vier],[c1, -zi, -zeh, -un],4),li([],[=c2, +taus_, c2],epsilon),li([sechs],[c1, -un],6),li([und],[ =cundZIG, =c1, +un, c2, -taus_],[A,B,Out] >> (Out = und(B,A))),li([],[=c1, +zi, c1],epsilon),li([hundert],[=c2, =c1, +un, c3, -taus],[A,B,Out] >> (Out = mal100plus(B,A))),li([],[=c2, +taus_, c2],epsilon),li([],[=c1, +zeh, c1],epsilon),li([],[=c3, +taus, c3],epsilon),li([hundert],[=c2, =c1, +un, c3, -taus],[A,B,Out] >> (Out = mal100plus(B,A))),li([tausend],[=c3, =c3, +taus, c4],[A,B,Out] >> (Out = mal1000plus(B,A)))],Sentance,Lambda,Tree).
%
% V1: 1293 mit Lambda
% makeLambdaSentance([li([neun],[c1, -zi, -zeh, -un],9),li([drei],[c1, -ssi, -zeh, -un],3),li([],[=c1, +zi, +zeh, +un, c1, -zi],epsilon),li([],[=c1, +ssi, c1],epsilon),li([zig],[=c1, +zi, cundZIG],[A,Out]>> (Out = mal10A(A))),li([],[=c1, +zeh, c1],epsilon),li([und],[ =cundZIG, =c1, +un, c2, -taus_],[A,B,Out] >> (Out = und(B,A))),li([],[=c2, +taus_, c2],epsilon),li([zwei],[c1, -un],2),li([hundert],[=c2, =c1, +un, c3, -taus],[A,B,Out] >> (Out = mal100plus(B,A))),li([],[=c3, +taus, c3],epsilon),li([eintausend],[=c3, c4],[A,Out] >> (Out = plus1000(A)))],Sentance,Lambda,Tree).
%
% V1: zweiundsiebzigtausenddreihunderzwölf mit Lambda (72312)
% makeLambdaSentance([li([drei],[c1,-ssi,-zeh,-un],3),li([],[=c1,+ssi,c1],epsilon),li([],[=c1,+zeh,c1],epsilon),li([zwoelf],[c2,-taus_],12),li([],[=c2,+taus_,c2],epsilon),li([hundert],[=c2,=c1,+un,c3,-taus],[A,B,Out]>>(Out is (B * 100 + A))),li([],[=c3,+taus,c3],epsilon),li([zwei],[c1,-un],2),li([undsiebzig],[=c1,+un,c2,-taus_],[A,Out]>>plus(A,70,Out)),li([],[=c2,+taus_,c3,-taus],epsilon),li([tausend],[=c3,=c3,+taus,c4],[A,B,Out]>>(Out is (B * 1000 + A)))],Sentance,Lmabda,Tree).
%
% makeLambdaSentance([li([drei],[c1,-ssi,-zeh,-un],3),li([],[=c1,+ssi,c1],epsilon),li([],[=c1,+zeh,c1],epsilon),li([zwoelf],[c2,-taus_],12),li([],[=c2,+taus_,c2],epsilon),li([hundert],[=c2,=c1,+un,c3,-taus],[A,B,Out]>>(Out = mal100plus(B,A))),li([],[=c3,+taus,c3],epsilon),li([zwei],[c1,-un],2),li([undsiebzig],[=c1,+un,c2,-taus_],[A,Out]>>(Out = plus70(A))),li([],[=c2,+taus_,c3,-taus],epsilon),li([tausend],[=c3,=c3,+taus,c4],[A,B,Out]>>(Out = mal1000plus(B,A)))],Sentance,Lmabda,Tree).
%
% makeLambdaSentance([li([zwoelf],[c2,-taus_],12),li([],[=c2,+taus_,c2],epsilon),li([drei],[c1,-ssi,-zeh,-un],3),li([],[=c1,+ssi,c1],epsilon),li([],[=c1,+zeh,c1],epsilon),li([zwei],[c1,-un],2),li([hundert],[=c2,=c1,+un,c3,-taus],[A,B,Out]>>(Out = mal100plus(B,A))),li([],[=c3,+taus,c3],epsilon),li([undsiebzig],[=c1,+un,c2,-taus_],[A,Out]>>(Out = plus70(A))),li([],[=c2,+taus_,c3,-taus],epsilon),li([tausend],[=c3,=c3,+taus,c4],[A,B,Out]>>(Out = mal1000plus(B,A)))],Sentance,Lmabda,Tree).
%
% V1: dreihundert mit Lambda
% makeLambdaSentance([li([drei],[c1,-ssi,-zeh,-un],3),li([],[=c1,+ssi,c1],epsilon),li([],[=c1,+zeh,c1],epsilon),li([hundert],[=c1,+un,c3,-taus],[A, Out]>>(Out is (A * 100))),li([],[=c3,+taus,c3],epsilon), li([],[=c3,c4],epsilon)],Sentance,Lambda,Tree).
%
% V1: dreihundertzwölf mit Lambda
% makeLambdaSentance([li([drei],[c1,-ssi,-zeh,-unU],3),li([],[=c1,+ssi,c1],[A,Out] >> (Out = A)),li([],[=c1,+zeh,c1],[A,Out] >> (Out = A)),li([],[=c1,+unU,c1,-un],[A,Out] >> (Out = A)),li([zwoelf],[c2,-tausU],12),li([],[=c2,+tausU,c2],[A,Out] >> (Out = A)),li([hundert],[=c2,=c1,+un,c3,-taus],[A,B, Out]>>(Out is (B * 100 + A))),li([],[=c3,+taus,c3],[A,Out] >> (Out = A)),li([],[=c3,c4],[A,Out] >> (Out = A))],Sentance,Lambda,Tree).
%
% V1: fourthousandonehundredandeighteen
% makeSentance([li([een],[=c1, +ee, c2]),li([eight],[c1, -ee]),li([hundredand],[=c2,=c1,c3]),li([one],[c1]),li([],[=c1,+tee,c1]),li([four],[c1,-tee]),li([],[=c1,c2]),li([],[=c2,c3]),li([thousand],[=c3,=c3,c4])], Sentance, Tree).
%
% V1: fourthousandonehundredandeighteen with lambda
% makeLambdaSentance([li([een],[=c1, +ee, c2],[B,Out]>>(Out is (0 * B + 18))),li([eight],[c1, -ee],8),li([hundredand],[=c2,=c1,c3],[A,B,Out]>>(Out is (B * 100 + A))),li([one],[c1],1),li([],[=c1,+tee,c1],epsilon),li([four],[c1,-tee],4),li([],[=c1,c2],epsilon),li([],[=c2,c3],epsilon),li([thousand],[=c3,=c3,c4],[A,B,Out]>>(Out is (B * 1000 + A)))], Sentance, Lambda, Tree).
%
% V1: sept cents quatre-vingt-trois
% makeSentance([li([],[=c1,+cent,c2,-xante_]),li([trois],[c1,-cent]),li([],[=c2,+xante_,c2]),li([],[=c1,+vi,+cent,c1,-vi]),li([quatre],[c1,-vi,-cent]),li([vingt],[=c2,=c1,+vi,c3]),li([],[=c1,+dix_,c1]),li([sept],[c1,-dix_,-cent]),li([cents],[=c3,=c1,+cent,c4,-mil]),li([],[=c4,+mil,c4]),li([],[=c4,c5])],Sentance,Tree).
%
% V1: cent sept mit Lambda
% makeLambdaSentance([li([sept],[c1,-dix_,-cent],7),li([],[=c1,+dix_,+cent,c1,-dix_],epsilon),li([],[=c1,+dix_,c1],epsilon),li([],[=c1,c2],epsilon),li([],[=c2,c3],epsilon),li([cents],[=c3,c4,-mil],[A,Out]>>(Out is (A + 100))),li([],[=c4,+mil,c4],epsilon)],Sentance,Lambda,Tree).
%
% V1: sept cents quatre-vingt-trois mit lambda
% makeLambdaSentance([li([trois],[c1,-cent],3),li([],[=c1,+cent,c2,-xante_],epsilon),li([],[=c2,+xante_,cxante_],epsilon),li([quatre],[c1,-vi,-cent],4),li([],[=c1,+vi,+cent,c1,-vi],epsilon),li([vingt],[=cxante_, =c1,+vi,c3],[A,B,Out]>>(Out is (A + B * 0 + 80))),li([sept],[c1,-dix_,-cent],7),li([],[=c1,+dix_,c1],epsilon),li([cents],[=c3,=c1,+cent,c4,-mil],[A,B,Out]>>(Out is (A + 100 * B))),li([],[=c4,+mil,c4],epsilon)],Sentance, Lambda,Tree).
%
% V1: shi wan yi qian ling yi shi er
% makeSentance([li([],[=c1,+sh,c1]),li([er],[c1,-sh]),li([shi],[=c1,c2,-yi1]),li([],[=c2,+yi1,cquian23]),li([],[=cquian23,=c1,+bai1shi,cquian22]),li([yi],[c1,-bai1shi]),li([],[=cquian22,=c0,cquian21]),li([ling],[c0]),li([],[=c1,+bai1shi,c1]),li([yi],[c1,-bai1shi]),li([qian],[=cquian21,=c1,c4,-wan31]),li([],[=c4,+wan31,cwan31]),li([],[=c2,+yi1,c2]),li([shi],[c2,-yi1]),li([],[=c2,c3]),li([],[=c3,c4]),li([wan],[=cwan31,=c4,c5])],Sentance,Tree).
%
% V1: shi wan yi qian ling yi shi er mit Lambda
% makeLambdaSentance([li([],[=c1,+sh,c1],epsilon),li([er],[c1,-sh],2),li([shi],[=c1,c2,-yi1],[A,Out]>>(Out is (A + 10))),li([],[=c2,+yi1,cquian23],[A,Out]>>(Out is (A * 1))),li([],[=cquian23,=c1,+bai1shi,cquian22],[A,B,Out]>>(Out is (A + B * 0))),li([yi],[c1,-bai1shi],1),li([],[=cquian22,=c0,cquian21],[A,B,Out]>>(Out is (A + B * 0))),li([ling],[c0],0),li([],[=c1,+bai1shi,c1],epsilon),li([yi],[c1,-bai1shi],1),li([qian],[=cquian21,=c1,c4,-wan31],[A,B,Out]>>(Out is (A + 1000*B))),li([],[=c4,+wan31,cwan31],[A,Out]>>(Out is (A*1))),li([],[=c2,+yi1,c2],epsilon),li([shi],[c2,-yi1],10),li([],[=c2,c3],epsilon),li([],[=c3,c4],epsilon),li([wan],[=cwan31,=c4,c5],[A,B,Out]>>(Out is (A + 10000 * B)))],Sentance,Lambda,Tree).
%
% V2: 863174 mit Lambda
% makeLambdaSentance([li([vier],[c1, -zi, -zeh, -un],4),li([drei],[c1, -ssi, -zeh, -un],3),li([],[=c1, +zi, c1],epsilon),li([],[=c1, +ssi, c1],epsilon),li([acht],[c1, -zi, -zeh, -un],8),li([],[=c1, +zeh, c1],epsilon),li([],[=c1, +zeh, c1],epsilon),li([],[=c1, +zi, c1],epsilon),li([undsiebzig],[=c1, +un, c2, -tausU],[A,Out] >> (Out = plus70(A))),li([undsechzig],[=c1, +un, c2, -tausU],[A,Out] >> (Out = plus60(A))),li([],[=c1, +zeh, c1],epsilon),li([],[=c2, +tausU, c2],epsilon),li([],[=c2, +tausU, c2],epsilon),li([],[=c1, +un, cun],epsilon),li([einhundert],[=c2, c3, -taus],[A,Out] >> (Out = plus100(A))),li([hundert],[=c2, =cun, c3, -taus],[A,B,Out] >> (Out = mal100plus(B,A))),li([],[=c3, +taus, c3],epsilon),li([],[=c3, +taus, ctaus],epsilon),li([tausend],[=c3, =ctaus, c4],[A,B,Out] >> (Out = mal1000plus(B,A)))],Sentance,Lambda,Tree).
%
% V2: 863174 mit Epsilon-Versuchen
% makeLambdaSentance([li([vier],[c1, -zi, -zeh, -un],4),li([drei],[c1, -ssi, -zeh, -un],3),li([],[=c1, +zi, c1],[A,Out] >> (Out = epsilon(A))),li([],[=c1, +ssi, c1],[A,Out] >> (Out = epsilon(A))),li([acht],[c1, -zi, -zeh, -un],8),li([],[=c1, +zeh, c1],[A,Out] >> (Out = epsilon(A))),li([],[=c1, +zeh, c1],[A,Out] >> (Out = epsilon(A))),li([],[=c1, +zi, c1],[A,Out] >> (Out = epsilon(A))),li([undsiebzig],[=c1, +un, c2, -tausU],[A,Out] >> (Out = plus70(A))),li([undsechzig],[=c1, +un, c2, -tausU],[A,Out] >> (Out = plus60(A))),li([],[=c1, +zeh, c1],[A,Out] >> (Out = epsilon(A))),li([],[=c2, +tausU, c2],[A,Out] >> (Out = epsilon(A))),li([],[=c2, +tausU, c2],[A,Out] >> (Out = epsilon(A))),li([],[=c1, +un, cun],[A,Out] >> (Out = epsilon(A))),li([einhundert],[=c2, c3, -taus],[A,Out] >> (Out = plus100(A))),li([hundert],[=c2, =cun, c3, -taus],[A,B,Out] >> (Out = mal100plus(B,A))),li([],[=c3, +taus, c3],[A,Out] >> (Out = epsilon(A))),li([],[=c3, +taus, ctaus],[A,Out] >> (Out = epsilon(A))),li([tausend],[=c3, =ctaus, c4],[A,B,Out] >> (Out = mal1000plus(B,A)))],Sentance,Lambda,Tree).
%
% x1s1x2s2x3s3x4s4 mit Lambda
% makeLambdaSentance([li([s4],[=c4,+l4,cS4],[A,Out]>>(Out is (A * 1000))),li([x4],[c4,-l4],4),li([s3],[=cS4,=c3,+l3,cS3],[A,B,Out]>>(Out is (A + 100*B))),li([x3],[c3,-l3],3),li([s2],[=cS3,=c2,+l2,cS2],[A,B,Out]>>(Out is (A + B*10))),li([x2],[c2,-l2],2),li([s1],[=cS2,=c1,+l1,cS1],[A,B,Out]>>(Out is (A + B * 1 + 0))),li([x1],[c1,-l1],1)],Sentance,Lambda,Tree).
%
% mouse eats chees
% makeSentance([li([mouse],[n]),li([cheese],[n,-k]),li([the],[=n,d,-k]),li([eat],[=n,v,-f]),li([],[=v,+k,=d,prep]),li(['-s'],[=prep,+f,+k,t]),li([],[=t,c])],Sentance,Tree).
%
% the mouse eats cheese mit Lambda
% makeLambdaSentance([li([mouse],[n],mouse),li([cheese],[n,-k],cheese),li([the],[=n,d,-k],epsilon),li([eat],[=n,v,-f],[X,Y,Out]>>(Out = eat(X,Y))),li([],[=v,+k,=d,prep],[P,Q,Out]>>(makeLambda(Q,P,Out))),li(['-s'],[=prep,+f,+k,t],epsilon),li([],[=t,c],epsilon)],Sentance,Lambda,Tree).
%
% check for SMC: fail -> SMC check correct, Sentance = acb -> SMC check incorrect
% makeLambdaSentance([li([b],[=d,=c,+f,+f,v],epsilon),li([a],[d,-f],epsilon),li([c],[c,-f],epsilon)],Sentance,Lambda,Tree).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Katze frisst Maus und Hund weis nix aus prädikatenlogischem Ausdruck
% makeFromExpr(und(frisst(katze,maus),weis(hund,nix)),Sentance,Lambda,Tree).
%
% Hund weis nix und Maus frisst Käse und Katze frisst Maus aus prädikatenlogischem Ausdruck
% makeFromExpr(und(weis(hund,nix),und(frisst(maus,kaese),frisst(katze,maus))),Sentance,Lambda,Tree).
%
%Maus frisst Käse und Katze frisst Maus und Hund weis nix
% makeFromExpr(und(und(frisst(maus,kaese),frisst(katze,maus)),weis(hund,nix)),Sentance,Lambda,Tree).
%
% Maus frisst Käse und Katze frisst Maus und Frau kennt Mann und Postbote kennt Besitzer
% makeFromExpr(und(und(frisst(maus,kaese),frisst(katze,maus)),und(kennt(mann,frau),kennt(besitzer,postbote))),Sentance,Lambda,Tree).
%
% der Postbote kennt den Besitzer ? aus prädikatenlogischem Ausdruck
% makeFromExpr(frage(kennt(der(postbote),den(besitzer))),Sentance,Lambda,Tree).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tests für Funktionsweise
%
% Test für Merge: Geht Sucher zum Gesuchten, oder der Gesuchte zum Sucher
% makeLambdaSentance([li([mouse],[n],mouse),li([cheese],[n],cheese),li([the],[=n,n],epsilon),li([eats],[=n,=n,c],[X,Y,Out]>>(Out = eat(X,Y)))],Sentance,Lambda,Tree).
%
% V1: vierzehnfünftausend, ist derzeit möglich (vom Lexikon her), sollte es aber nicht
% makeLambdaSentance([li([vier],[c1, -zi, -zeh, -un],4),li([],[=c1, +zi, c1],epsilon),li([],[=c1, +zeh, +un, c1, -zeh],epsilon),li([zehn],[=c1, +zeh, c2, -taus_],[A,Out] >> (Out = plus10(A))),li([],[=c2, +taus_, c3, -taus],epsilon),li([fuenf],[c1, -zi, -zeh, -un],5),li([],[=c1, +zi, +zeh, +un, c1, -zi],epsilon),li([],[=c1, +zi, c1],epsilon),li([],[=c1, c2],epsilon),li([],[=c2, c3],epsilon),li([tausend],[=c3, =c3, +taus, c4],[A,B,Out] >> (Out = mal1000plus(B,A)))],Sentance,Lambda,Tree).

% Test for the finish check
% makeLambdaSentance([lambdaLi([drei],[c1,-ssi,-zeh,-un],3,4,[021]),lambdaLi([],[=c1,+ssi,c1],epsilon,4,[021]),lambdaLi([],[=c1,+zeh,c1],epsilon,4,[021]),lambdaLi([zwoelf],[c2,-taus_],12,4,[021]),lambdaLi([],[=c2,+taus_,c2],epsilon,4,[021]),lambdaLi([hundert],[=c2,=c1,+un,c3,-taus],[A,B, Out]>>(Out is (B * 100 + A)),4,[021]),lambdaLi([],[=c3,+taus,c3],epsilon,4,[021]),lambdaLi([],[=c3,c4],epsilon,4,[021])],Sentance,Lambda,Tree).

% Test für vollständige Generation
% die Funktionieren sollten
% deutsche Zahlen
% makeFromExprWithFilling(1000,Sentance,Lambda,Tree).                     % funktiniert
% makeFromExprWithFilling(1,Sentance,Lambda,Tree).                        % funktiniert
% makeFromExprWithFilling(2,Sentance,Lambda,Tree).                        % funktioniert
% makeFromExprWithFilling(plus70(3),Sentance,Lambda,Tree).                % funktioniert
% makeFromExprWithFilling(mal1000(3),Sentance,Lambda,Tree).               % funktioniert
% makeFromExprWithFilling(mal1000(plus70(3)),Sentance,Lambda,Tree).       % funktioniert
% makeFromExprWithFilling(mal1000plus(9,2),Sentance,Lambda,Tree).         % funktioniert
% makeFromExprWithFilling(mal1000plus(9,plus20(2)),Sentance,Lambda,Tree). % funktioniert
% makeFromExprWithFilling(mal1000plus(plus100(9),2),Sentance,Lambda,Tree).  % funktioniert
% makeFromExprWithFilling(mal1000plus(mal100(9),2),Sentance,Lambda,Tree). % funktioniert
% makeFromExprWithFilling(mal1000plus(mal100plus(3,plus60(9)),mal100plus(2,12)),Sentance,Lambda,Tree). % funktioniert

% englische zahlen
% makeFromExprWithFilling(1,Sentance,Lambda,Tree).                    % funktioniert <- NB: überprüfe Fill-in, abhängig von Reihenfolge der Epsilon-LI
% makeFromExprWithFilling(4,Sentance,Lambda,Tree).                    % funktioniert
% makeFromExprWithFilling(15,Sentance,Lambda,Tree).                   % funktioniert
% makeFromExprWithFilling(plus50(9),Sentance,Lambda,Tree).            % funktioniert
% makeFromExprWithFilling(mal100(3,epsilon),Sentance,Lambda,Tree).    % funktioniert
% makeFromExprWithFilling(mal10(4),Sentance,Lambda,Tree).             % funktioniert
% makeFromExprWithFilling(mal10plus(4,9),Sentance,Lambda,Tree).       % funktioniert
% makeFromExprWithFilling(mal100plus(2,50),Sentance,Lambda,Tree).     % funktioniert
% makeFromExprWithFilling(mal1000(9,epsilon),Sentance,Lambda,Tree).   % funktioniert (Bsp, wo eine "Brücke" notwendig ist)
% makeFromExprWithFilling(mal1000plus(mal100plus(3,plus50(9)),mal100plus(2,12)),Sentance,Lambda,Tree). % funktioniert

% einfache Maus Bsp
% makeFromExprWithFilling(frisst(maus,kaese),Sentance,Lambda,Tree). % funktioniert, zeigt, dass auch Y-LI mit nicht leeren phonetischen Strings

% Zahlen im Kontext
% makeFromExprWithFilling(gibt(modify(plus70(4),banane)),Sentance,Lambda,Tree). % funktioniert
% makeFromExprWithFilling(liegt(modify(3,kaese),modifyLoc(feld(xBy(1,2)))),Sentance,Lambda,Tree). % funktioniert
% makeFromExprWithFilling(liegt(modifyLoc(feld(xBy(2,4))),banane),Sentance,Lambda,Tree). % funktioniert

% makeFromExprWithFilling(eat(mouse,modifyLoc(cheese,field(xBy(2,3)))),Sentance,Lambda,Tree).
% makeFromExprWithFilling(liegt(modifyLoc(feld(xBy(1,2))),modify(3,kaese)),Sentance,Lambda,Tree). % funktioniert noch nicht

% numbers with context
% makeFromExprWithFilling(lie(modify(3,cheese),modifyLoc(field(xBy(1,2)))),Sentance,Lambda,Tree). % funktioniert
% makeFromExprWithFilling(lie(cheese,loc(field(xBy(1,2)))),Sentance,Lambda,Tree). %funktioniert
% makeFromExprWithFilling(find(mouse,loc(cheese,corner)),Sentance,Lambda,Tree). %funktioniert NB: noch problem wenn Lexikon für Beispiel unten ausgerichtet ist
% makeFromExprWithFilling(find(mouse,loc(cheese,field(xBy(1,2)))),Sentance,Lambda,Tree). %funktioniert NB:noch problem wenn Lexikon für Beispiel oben ausgerichtet ist
% makeFromExprWithFilling(lie(modify(mal10plus(4,3),cheese),modifyLoc(field(xBy(1,2)))),Sentance,Lambda,Tree). % funktioniert
% makeFromExprWithFilling(eat(mouse,modifyLoc(cheese,field(xBy(2,3)))),Sentance,Lambda,Tree). % funktioniert, Bsp. dafür, dass zwischen Funktionen mit unterschiedlich langen Skopusen unterschieden wird
% makeFromExprWithFilling(eat(mouse,modifyLoc(cheese,field(xBy(mal10plus(6,9),mal10plus(4,2))))),Sentance,Lambda,Tree). % funktioniert, Bsp. dafür, dass zwischen Funktionen mit unterschiedlich langen Skopusen unterschieden wird
% makeFromExprWithFilling(eat(mouse,cheese),Sentance,Lambda,Tree). % funktioniert, zeigt, dass auch Y-LI mit nicht leeren phonetischen Strings

% makeFromExprWithFilling(lie(modifyLoc(field(xBy(1,2))),modify(mal10plus(4,3),cheese)),Sentance,Lambda,Tree). % funktioniert noch nicht

% soll nicht funktionieren
% deutsche Zahlen Wörter
% makeFromExprWithFilling(mal1000plus(9,plus20(zwiebel)),Sentance,Lambda,Tree). % "Zwiebel" kein Teil der Grammatik <- NB: hier müssen noch schräfere Abbruchbedingung programmiert werden
% makeFromExprWithFilling(mal1000plus(9,plus20(2),7),Sentance,Lambda,Tree).     % mal1000plus hat weniger Inputs als hier verwendet (sollte vom lambda-Select gefunden werden, aber auch vom ypsilon-Select)<- NB: hier müssen noch schräfere Abbruchbedingung programmiert werden
% makeFromExprWithFilling(mal100(C),Sentance,Lambda,Tree).                      % keine vollständige Ausdrücke (interessant für später)

% englische Zahlenwörter
% makeFromExprWithFilling(mal10(3),Sentance,Lambda,Tree).                       % sollte den wert 13 haben, aber nicht mit diesem Ausdruck, sondern mit 13
% makeFromExprWithFilling(mal1000plus(mal100plus(plus50(9),3),mal100plus(12,2)),Sentance,Lambda,Tree). % hat in den mal100plus die Werte vertauscht, es sagt zwar [three,hundredand,fifty,nine,thousand,two,hundredand,twelve], wie auch sein richtiges Pendante, aber die Semantik ist anders
% makeFromExprWithFilling(plus50(20),Sentance,Lambda,Tree).                     % 20 ist kein legaler input in plus50(A)
% makeFromExprWithFilling(mal100(epsilon,2),Sentance,Lambda,Tree).              % falsche Reihenfolge der 2 und epsilon

% makeFromExprWithFilling(find(hungry(mouse),loc(cheese,field(bFa(b,t10plus(6,4))))),Sentance,Lambda,Tree).
