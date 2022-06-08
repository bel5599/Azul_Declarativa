
add(X,[],[X]).
add(X,[Y|Z],[Y|W]):-add(X,Z,W).

extraer_columna([], _ ,[]):-!.
extraer_columna([X|Xs],Indice,[Y|Ys]):-
      nth0(Indice,X,Y),
      extraer_columna(Xs,Indice,Ys).
