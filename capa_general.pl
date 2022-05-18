/*Tiene que haber un metodo principal que es el que llama a todos los metodos a ejecujatarse, para saber el conteo de los azulejos, puntuacion y demas
Ademas cuando empiece el primero, en cadena, debe seguirle el segundo con la informacion del primero, para tener todo actualizado*/

Azulejo(1,rojo).
Azulejo(2,amarillo).
Azulejo(3,blanco).
Azulejo(4,negro).
Azulejo(5,azul).

/*Inicio del juego
segun la cantidad de jugadores, la cantidad de losetas de fabricas que tocan
Inicio_Simulacion_Jugadores_Losetas(cantidad_jugadores,cantidad_losetas_de_fabricas)*/
Inicio_Simulacion_Jugadores_Losetas(2,5).
Inicio_Simulacion_Jugadores_Losetas(3,7).
Inicio_Simulacion_Jugadores_Losetas(4,9).

Inicio_Simulacion(X,W):-Inicio_Simulacion_Jugadores_Losetas(X,Y), Llenar_Losetas(Y,Z).

Llenar_Losetas(0,[]):-!.
Llenar_Losetas(X,[Y]):-X1 is X-1, Asignacion_Azulejos_Loseta(_,[W]), Push(W,Y,Z),Llenar_Losetas(X1,Z).

Push(X,Y,W):-

Asignacion_Azulejos_Loseta(_,[Y]):-random(1,5,Y1),Asignar_color(Y1,X1),random(1,5,Y2),Asignar_color(Y2,X2),random(1,5,Y3),Asignar_color(Y3,X3),
                                   random(1,5,Y4),Asignar_color(Y4,X4),Y=[X1,X2,X3,X4].

Asignar_color(X,Y):-Azulejo(X,Y).

%M1: matriz donde se encuentra la ubicacion de los colores
%M2: matriz que guarda si ya hay algun azulejo en la posicion x,y
Muro([[azul,amarillo,rojo,negro,blanco],
      [blanco,azul,amarillo,rojo,negro],
      [negro,blanco,azul,amarillo,rojo],
      [rojo,negro,blanco,azul,amarillo],
      [amarillo,rojo,negro,blanco,azul]],

      [[0,0,0,0,0],
      [0,0,0,0,0],
      [0,0,0,0,0],
      [0,0,0,0,0],
      [0,0,0,0,0]]).

Muro(M1,M2,x,y,M2):-

/*predicado donde se comprueba si se puede ubicar un azulejo del color correspondiente
% en la posicion entrante
A- color del azulejo, [X,Y] - posicion, [MP]-matriz de preparacion,[MM]- matriz del muro, W true o false   FALTAN COSAS AQUI*/
Azulejo_en_posicion_valida(A,[X,Y],[MP],[M1],[M2],W):-Posicion_valida_en_zona_preparacion(X,Y,1), Recorrer_fila_zona_preparacion(A,[MP],1),Recorrer_fila_muro(A,[M1],[M2],1).

/*metodo que te devuelve la fila i en una matriz*/
Retornar_fila([X],0,0,[X]):-!.
Retornar_fila([X|Y],P,P1,W):-(P==P1,W=X,P2=P1+1);(Retornar_fila([Y],P,P2,W1),W is W1).

/*verificar que en la fila donde se va a poner la ficha las demas que esten sean del mismo color*/
Recorrer_fila_zona_preparacion(A,[],1):-!.
Recorrer_fila_zona_preparacion(A,[X],0):-A=\=X,!.
Recorrer_fila_zona_preparacion(A,[X|M],W):-Recorrer_fila_zona_preparacion(A,[M],W1), W is W1.

Posicion_valida_en_zona_preparacion(X,Y,W):-(X==1,Y==5,W=1,!);(X==2,(Y==4;Y==5),W=1,!);(X==3,(Y==3;Y==4;Y==5),W=1,!);(X==5,Y=<5,Y>=1,W=1,!).

/*Recorre una fila y va comparando si se encuentra el elemento*/
Recorrer_fila_muro(X,[],[],0):-!.
Recorrer_fila_muro(X,[X1|Y],[X2|Y1],W):-X==X1,X2==0,W=1,!;X==X1,X2==1,W=0,!;Recorrer_fila(X,Y,Y1,W1).

/*todas las posibles combinaciones*/
Espacio_preparacion([],[],[],[],[]).
Espacio_preparacion(L1,L2,L3,L4,L5).

Suelo([null,null,null,null,null,null,null]).

