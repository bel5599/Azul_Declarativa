

:-dynamic muro/3,
          espacio_preparacion/6,
          suelo/8,
          puntuacion/2,
          total_de_Azulejos/5,
          centro/1,
          loseta/2.

/*Tiene que haber un metodo principal que es el que llama a todos los metodos a ejecujatarse, para saber el conteo de los azulejos, puntuacion y demas
Ademas cuando empiece el primero, en cadena, debe seguirle el segundo con la informacion del primero, para tener todo actualizado*/

azulejo(1,rojo).
azulejo(2,amarillo).
azulejo(3,blanco).
azulejo(4,negro).
azulejo(5,azul).

/*representa la cantidad total de cada color de azulejo
                  R, A, B, N, A*/
total_de_Azulejos(20,20,20,20,20).

/*puntuacion(Jugador,0).

espacio_preparacion(Jugador,[],[],[],[],[]).

suelo(Jugador,[null,null,null,null,null,null,null]).

%M1: matriz donde se encuentra la ubicacion de los colores
%M2: matriz que guarda si ya hay algun azulejo en la posicion x,y
muro(Jugador,[[azul,amarillo,rojo,negro,blanco],
      [blanco,azul,amarillo,rojo,negro],
      [negro,blanco,azul,amarillo,rojo],
      [rojo,negro,blanco,azul,amarillo],
      [amarillo,rojo,negro,blanco,azul]],

      [[0,0,0,0,0],
      [0,0,0,0,0],
      [0,0,0,0,0],
      [0,0,0,0,0],
      [0,0,0,0,0]]).*/


/*Inicio del juego
segun la cantidad de jugadores, la cantidad de losetas de fabricas que tocan
Inicio_Simulacion_Jugadores_Losetas(cantidad_jugadores,cantidad_losetas_de_fabricas)*/
inicio_Simulacion_Jugadores_Losetas(2,5).
inicio_Simulacion_Jugadores_Losetas(3,7).
inicio_Simulacion_Jugadores_Losetas(4,9).

inicio_Simulacion(Jugadores,Losetas):-
      inicio_Simulacion_Jugadores_Losetas(Jugadores,Cantidad_Losetas),
      llenar_Losetas([Cantidad_Losetas]),/*crear un array con las losetas*/
      crear_centro(),
      asignar_Tablero_Jugador([Jugadores]), /*falta crear un array con los jugadores*/
      ficha_Jugador_Inicial(X1,Jugadores).

/*COMPLETAR*/
desarrollo_de_la_Partida():-fase_I(Jugadores,Jugador_Inicial,Cantidad_Losetas),fase_II(),fase_III().

crear_centro():-
      asserta(centro([])).

fase_I(Jugadores,Jugador_Inicial,Cantidad_Losetas):-
      seleccion_Loseta(Jugador_Inicial,Cantidad_Losetas,Loseta),
      actualizar_Loseta_Tablero_Centro(Jugador_Inicial,Loseta),
      fase_I().
seleccion_Loseta(Jugador,Losetas,Loseta).

actualizar_Loseta_Tablero_Centro(Jugador,Loseta):-
      loseta(Loseta,[Azulejo|Resto]),
      seleccion_Azulejos(Jugador,[Azulejo|Resto]).

seleccion_Azulejos(Jugador,[X|Resto]):-
      pasar_Azulejo_tablero(X,Jugador,Y),
      Y is 'False',
      centro(Azulejos_Centro),
      retract(centro(Azulejos_Centro)),
      push(X,Azulejos_Centro,Azulejos_Centro_Actualizado),
      asserta(centro(Azulejos_Centro_Actualizado)),
      seleccion_Azulejos(Jugador,[Resto]).

pasar_Azulejo_tablero(X,Jugador,Y).

fase_II([J1|Resto]):-
      espacio_preparacion(J1,[F1],[F2|Resto_F2],[F3|Resto_F3],[F4|Resto_F4],[F5|Resto_F5]),
      [F1] =\= [],
      Color is F1,

      .

ubicar_Azulejo_Muro(Color,Jugador, Fila_Actual):-
      muro(Jugador,Tablero_Colores,Tablero_boleano),
      ((Color == rojo,
      Position = 3);
      (Color == amarillo,
      Position = 2);
      (Color == azul,
      Position = 1);
      (Color == blanco,
      Position = 5);
      (Color == negro,
      Position = 4)),
      nth0(Fila_Actual,Tablero_boleano,Fila_a_modificar),
      crear_fila_actualizada_muro(Fila_a_modificar,0,Position,Fila_actualizada),
      insert(Tablero_boleano,Fila_Actual,Fila_actualizada,Tablero_boleano_Actualizado).

crear_fila_actualizada_muro(Fila,Posicion,Posicion_a_modificar,Fila):-
      Posicion==Posicion_a_modificar,
      insert()


insert([X|Y],position_inicial,position,[W]):-
      position_inicial==position,
      X is 1,
      
fase_III().

ficha_Jugador_Inicial(X,Jugadores):-
      random(1,5,X),
      asserta(Jugador_Inicial,X).

llenar_Losetas([]):-!.
llenar_Losetas([Loseta|Resto]):-
      asignacion_Azulejos_Loseta([W]),
      asserta(loseta(Loseta,[W])),
      llenar_Losetas([Resto]).

push(X,Y,[X|Y]).

vaciar_Loseta(Numero_Loseta,Losetas).    

asignacion_Azulejos_Loseta([Loseta_Llena]):-
      random(1,5,Y1),
      asignar_color(Y1,X1),
      actualizar_cantidad_Azulejos(Y1),
      random(1,5,Y2),
      asignar_color(Y2,X2),
      actualizar_cantidad_Azulejos(Y2),
      random(1,5,Y3),
      asignar_color(Y3,X3),
      actualizar_cantidad_Azulejos(Y3),
      random(1,5,Y4),
      asignar_color(Y4,X4),
      actualizar_cantidad_Azulejos(Y4),
      Loseta_Llena=[X1,X2,X3,X4].

asignar_color(X,Y):-azulejo(X,Y).

actualizar_cantidad_Azulejos(Color_de_Azulejo):-
      total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,Azules),
      retract(total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,Azules)),
      (Color_de_Azulejo==1,
      RojosN is Rojos-1,
      asserta(total_de_Azulejos(RojosN,Amarillos,Blancos,Negros,Azules)));
      (Color_de_Azulejo==2,
      AmarillosN is Amarillos-1,
      asserta(total_de_Azulejos(Rojos,AmarillosN,Blancos,Negros,Azules)));
      (Color_de_Azulejo==1,
      BlancosN is Blancos-1,
      asserta(total_de_Azulejos(Rojos,Amarillos,BlancosN,Negros,Azules)));
      (Color_de_Azulejo==1,
      NegrosN is Negros-1,
      asserta(total_de_Azulejos(Rojos,Amarillos,Blancos,NegrosN,Azules)));
      (Color_de_Azulejo==1,
      AzulesN is Azules-1,
      asserta(total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,AzulesN))).
      
      

asignar_Tablero_Jugador(0):-!.
asignar_Tablero_Jugador([J|Resto]):-
      asserta(puntuacion(J,0)),
      asserta(muro(J,[[azul,amarillo,rojo,negro,blanco],
          [blanco,azul,amarillo,rojo,negro],
          [negro,blanco,azul,amarillo,rojo],
          [rojo,negro,blanco,azul,amarillo],
          [amarillo,rojo,negro,blanco,azul]],
  
          [[0,0,0,0,0],
          [0,0,0,0,0],
          [0,0,0,0,0],
          [0,0,0,0,0],
          [0,0,0,0,0]])),
      asserta(espacio_preparacion(J,[],[],[],[],[])),
      asserta(suelo(J,[null,null,null,null,null,null,null])),
      write(puntuacion(J,0)),
      asignar_Tablero_Jugador([Resto]).

tablero(Puntuacion,espacio_preparacion(L1,L2,L3,L4,L5),muro(M1,M2),suelo(S1)).



/*predicado donde se comprueba si se puede ubicar un azulejo del color correspondiente
% en la posicion entrante
A- color del azulejo, [X,Y] - posicion, [MP]-matriz de preparacion,[MM]- matriz del muro, W true o false   FALTAN COSAS AQUI*/
azulejo_en_posicion_valida(A,[X,Y],[MP],[M1],[M2],W):-
      posicion_valida_en_zona_preparacion(X,Y,1), 
      recorrer_fila_zona_preparacion(A,[MP],1),
      recorrer_fila_muro(A,[M1],[M2],1).

/*metodo que te devuelve la fila i en una matriz*/
retornar_fila([X],0,0,[X]):-!.
retornar_fila([X|Y],P,P1,W):-
      (P==P1,W=X,P2=P1+1);
      (retornar_fila([Y],P,P2,W1),
      W is W1).

/*verificar que en la fila donde se va a poner la ficha las demas que esten sean del mismo color*/
recorrer_fila_zona_preparacion(A,[],1):-!.

recorrer_fila_zona_preparacion(A,[X],0):-
      A=\=X,!.

recorrer_fila_zona_preparacion(A,[X|M],W):-
      recorrer_fila_zona_preparacion(A,[M],W1),
      W is W1.

posicion_valida_en_zona_preparacion(X,Y,W):-
      (X==1,Y==5,W=1,!);
      (X==2,(Y==4;Y==5),W=1,!);
      (X==3,(Y==3;Y==4;Y==5),W=1,!);
      (X==5,Y=<5,Y>=1,W=1,!).

/*Recorre una fila y va comparando si se encuentra el elemento*/
recorrer_fila_muro(X,[],[],0):-!.
recorrer_fila_muro(X,[X1|Y],[X2|Y1],W):-
      X==X1,
      X2==0,
      W=1,!;
      X==X1,
      X2==1,
      W=0,!;
      recorrer_fila_muro(X,Y,Y1,W1).


