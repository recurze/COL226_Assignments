member(X,[X|Y]).
member(X,[Y|T]) :- member(X,T).
