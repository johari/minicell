%% A machine (M) is a quadruple like
    %% (Q)
    %% (\delta)
    %% (\Sigma)
    %% (s)
    %% (F)

%% We need to define (Q) and (\delta)

moveDown(colRowAddr(Col, Row), colRowAddr(Col, RR)) :- RR is Row+1.

delta(idleMode(Addr), keyPress(down), idleMode(NewAddr)) :-
    miniprint("moving down from #{Addr} to #{NewAddr}"),
    moveDown(Addr, NewAddr).

miniprint(X) :- print(X), nl.

splashScreen(X, X)                                 :- miniprint("spashScreen").
minicellInterface(X, idleMode(colRowAddr('A', 1))) :- miniprint("Minicell Interface").

main(S, E) :-
    splashScreen(S, Y),
    minicellInterface(Y, E).

figure1(E) :-
    main(_, S),
    delta(S, keyPress(down), SS),
    delta(SS, keyPress(down), E).
