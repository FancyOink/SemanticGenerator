startCategory(c4).
[zig] :: ([=c1, +zi, c2, -tausU],[A,Out]>> (Out = mal10(A))). % 10 * A	%Hauptfunktionen
[zig] :: ([=c1, +zi, cundZIG],[A,Out]>> (Out = mal10A(A))). % 10 * A
[und] :: ([ =cundZIG, =cun, c2, -tausU],[A,B,Out] >> (Out = und(B,A))). % A + B
[zehn] :: ([=c1, +zeh, c2, -tausU],[A,Out] >> (Out = plus10(A))). % A + 10
[hundert] :: ([=c2, =cun, c3, -taus],[A,B,Out] >> (Out = mal100plus(B,A))). % A + 100 * B
[hundert] :: ([=c1, +un, c3, -taus],[A,Out] >> (Out = mal100(A))). % 100 * A
[einhundert] :: ([=c2, c3, -taus],[A,Out] >> (Out = plus100(A))). % A + 100
[einhundert] :: ([c3, -taus],100). % 100
[tausend] :: ([=c3, =c3,+taus, c4],[A,B,Out] >> (Out = mal1000plus(B,A))). % A + 1000 * B
[tausend] :: ([=c3, +taus, c4],[A,Out] >> (Out = mal1000(A))). % 1000 * A
[eintausend] :: ([=c3, c4],[A,Out] >> (Out = plus1000(A))). % A + 1000
[eintausend] :: ([c4],1000). % 1000

[eins] :: ([c1],1).	% 1		%c1
[zwei] :: ([c1, -unU],2). % 2
[drei] :: ([c1, -ssi, -zeh, -unU],3). % 3
[vier] :: ([c1, -zi, -zeh, -unU],4). % 4
[fuenf] :: ([c1, -zi, -zeh, -unU],5). % 5
[sechs] :: ([c1, -unU],6). % 6
[sieben] :: ([c1, -unU],7). % 7
[acht] :: ([c1, -zi, -zeh, -unU],8). % 8
[neun] :: ([c1, -zi, -zeh, -unU],9). % 9
[zehn] :: ([c2, -tausU],10). % 10		%diverse Ausnahmen
[elf] :: ([c2, -tausU],11). % 11
[zwoelf] :: ([c2, -tausU],12). % 12
[sechzehn] :: ([c2, -tausU],16). % 16
[siebzehn] :: ([c2, -tausU],17). % 17
[zwanzig] :: ([c2, -tausU],20). % 20
[einundzwanzig] :: ([c2, -tausU],21). % 21
[sechzig] :: ([c2, -tausU],60). % 60
[einundsechzig] :: ([c2, -tausU],61). % 61
[siebzig] :: ([c2, -tausU],70). % 70
[einundsiebzig] :: ([c2, -tausU],71). % 71
[undzwanzig] :: ([=c1, +un, c2, -tausU],[A,Out] >> (Out = plus20(A))). % A + 20
[ssig] :: ([=c1, +ssi, c2, -tausU],[A,Out] >> (Out = plus30(A))). % 0 * A + 30
[ssig] :: ([=c1, +ssi, cundZIG],[A,Out] >> (Out = plus30A(A))). % 0 * A + 30
[einund] :: ([=cundZIG, c2, -tausU],[A,Out] >> (Out = plus1(A))). % A + 1
[undsechzig] :: ([=c1, +un, c2, -tausU],[A,Out] >> (Out = plus60(A))). % A + 60
[undsiebzig] :: ([=c1, +un, c2, -tausU],[A,Out] >> (Out = plus70(A))). % A + 70


[] :: ([=c1, +unU, cun],[A,Out] >> (Out = A)).
[] :: ([=c1, +unU,c1,-un],[A,Out] >> (Out = A)).
[] :: ([=c3, +taus, ctaus],[A,Out] >> (Out = A)).
[] :: ([=c1, +zi, c1],[A,Out] >> (Out = A)). % epsilon		%Schminke
[] :: ([=c1, +zi, +zeh, +unU, c1, -zi],[A,Out] >> (Out = A)). % epsilon
[] :: ([=c1, +ssi, c1],[A,Out] >> (Out = A)). % epsilon
[] :: ([=c1, +ssi, +zeh, +unU, c1, -ssi],[A,Out] >> (Out = A)). % epsilon
[] :: ([=c1, +zeh, c1],[A,Out] >> (Out = A)). % epsilon
[] :: ([=c1, +zeh, +unU, c1, -zeh],[A,Out] >> (Out = A)). % epsilon
[] :: ([=c1, +unU, c1],[A,Out] >> (Out = A)). % epsilon
[] :: ([=c2, +tausU, c2],[A,Out] >> (Out = A)). % epsilon
[] :: ([=c3, +taus, c3],[A,Out] >> (Out = A)). % epsilon
[] :: ([=c3, c4],[A,Out] >> (Out = A)). % epsilon		%Upgrader
[] :: ([=c2, +tausU, c3, -taus],[A,Out] >> (Out = A)). % epsilon
[] :: ([=c2, c3],[A,Out] >> (Out = A)). % epsilon
[] :: ([=c1, +unU, c2, -tausU],[A,Out] >> (Out = A)). % epsilon
[] :: ([=c1, c2],[A,Out] >> (Out = A)). % epsilon

% hinzugefügt, um reine Suche anhand hinzugefügter Features durch Y-Li zu vermeiden könnte vieleicht auch durch eine "Vorverarbeitung" des Lexikons aus den anderen Y-LI generiert werden.
[] :: ([=c1, c3],[A,Out] >> (Out = A)).
[] :: ([=c2, c4],[A,Out] >> (Out = A)).
[] :: ([=c1, c4],[A,Out] >> (Out = A)).
