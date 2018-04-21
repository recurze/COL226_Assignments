% ht stands for hastype: (G, E, T); true if Type(E)=T.
% :%s/ht/hastype/g
% te stands for typeElaborate: (G, D, G'); true if G' is (D augmented G)
% :%s/te/typeElaborate/g
% dv stands for definedVariables: (D, L); true if L is list of dv(D).
% :%s/dv/definedVariables/g

% lookup: (L, F, S); true if (F, S) in L. Should have just used member -_-
% fixed ^
% ith: (I, L, Ei); true if L[I]=Ei. For projection
% member: (X, L); true if X is in L.
% intersection: (L1, L2); true if L1, L2 don't have any common elements.

% A check to see if it is boolean or not.
bool(true):- !.
bool(false):- !.

v(X):- string(X).

member(_, []):- fail.
member(X, [X|_]):- !.
member(X, [_|Ys]):- member(X, Ys).

% base types: intT, boolT; derived: T1->T2(arrowT), T1*T2...*Tn(cartesianT)
ht(G, v(X), T):- member(p(v(X), T), G).
ht(_, const(X), intT):- integer(X).
ht(_, bool(X), boolT):- bool(X).

% arith operations over integers.
ht(G, abs(E), intT):- ht(G, E, intT).
ht(G, add(E1, E2), intT):- ht(G, E1, intT), ht(G, E2, intT).
ht(G, sub(E1, E2), intT):- ht(G, E1, intT), ht(G, E2, intT).
ht(G, mul(E1, E2), intT):- ht(G, E1, intT), ht(G, E2, intT).
ht(G, div(E1, E2), intT):- ht(G, E1, intT), ht(G, E2, intT).
ht(G, moD(E1, E2), intT):- ht(G, E1, intT), ht(G, E2, intT).
ht(G, pow(E1, E2), intT):- ht(G, E1, intT), ht(G, E2, intT).

% boolean/logical operations on bools
ht(G, noT(E), boolT):- ht(G, E, boolT).
ht(G, anD(E1, E2), boolT):- ht(G, E1, boolT), ht(G, E2, boolT).
ht(G, orr(E1, E2), boolT):- ht(G, E1, boolT), ht(G, E2, boolT).
ht(G, imply(E1, E2), boolT):- ht(G, E1, boolT), ht(G, E2, boolT).

% comparison of integers
ht(G, great(E1, E2), boolT):- ht(G, E1, intT), ht(G, E2, intT).
ht(G, lesss(E1, E2), boolT):- ht(G, E1, intT), ht(G, E2, intT).
ht(G, grequ(E1, E2), boolT):- ht(G, E1, intT), ht(G, E2, intT).
ht(G, leequ(E1, E2), boolT):- ht(G, E1, intT), ht(G, E2, intT).

% equality for integers and bools
ht(G, equal(E1, E2), boolT):-
    (ht(G, E1, intT), ht(G, E2, intT));
    (ht(G, E1, boolT), ht(G, E2, boolT)).

% conditional expressions (ite)
ht(G, ite(E0, E1, E2), T):- ht(G, E0, boolT), ht(G, E1, T), ht(G, E2, T).

% qualified expressions of the form let D in E
ht(G, letin(D, E), T):- te(G, D, GG), append(G, GG, G_), ht(G_, E, T).

% abstraction,
ht(G, lambda(v(X), E), arrowT(T1,T2)):-ht([p(v(X), T1)|G], E, T2).

% function application
ht(G, apply(E1, E2), T2):- ht(G, E2, T1), ht(G, E1, arrowT(T1, T2)).

% n-tuples
ht(G, tuple([]), cartesianT([])):- true.
ht(G, tuple([X|Xs]), cartesianT([T|Ts])):-
    ht(G, X, T), ht(G, tuple(Xs), cartesianT(Ts)).

% projection, needs to be changed a little I think.
ht(G, proj(X, tuple(E)), T):- integer(X), ith(X, E, Ei), ht(G, Ei, T).

ith(0, [T|_], T):- !.
ith(X, [_|Ts], T):- C is X-1, ith(C, Ts, T).

% typeElaborations
te(G, def(v(X), E), GG):- ht(G, E, T), append([p(v(X), T)], G, GG).

te(G, seq(D1, D2), GG):-
    te(G, D1, G1), append(G1, G, G_), te(G_, D2, G2),
    append(G2, G1, GG).

te(G, pll(D1, D2), GG):-
    dv(D1, L1), dv(D2, L2), intersection(L1, L2),
    te(G, D1, G1), te(G, D2, G2),
    append(G1, G2, GG).

te(G, loc(D1, D2), GG):- te(G, D1, G1), append(G1, G, G_), te(G_, D2, GG).

% defined variables.
dv(def(v(X), _), L):- append([X], [], L).
dv(seq(D1, D2), L):- dv(D1, L1), dv(D2, L2), append(L1, L2, L).
dv(pll(D1, D2), L):- dv(D1, L1), dv(D2, L2), intersection(L1, L2), append(L1, L2, L).
dv(loc(_, D2), L):- dv(D2, L2), append([], L2, L).

% to check if intersection is {}. \+ is for negation it seems.
intersection([], _):- !.
intersection([X|L], L1):- \+ member(X, L1), intersection(L, L1).
