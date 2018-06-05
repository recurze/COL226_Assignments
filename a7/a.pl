croaks("fritz").
croaks("fri").
eats("fritz", "flies").
eats("fritz", "fritz").

frog(X):-
    eats(X, Y),
    croaks(Y).

canary(X):-
    chirps(X),
    sings(X).

green(X):-
    frog(X).

yellow(X):-
    canary(X).
