concat(nil,A,A).
concat(con(T,Z),A,con(T,P)) :- concat(Z,A,P).

