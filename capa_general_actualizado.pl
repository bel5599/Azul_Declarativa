

:-dynamic muro/3,
          espacio_preparacion/6,
          suelo/8,
          puntuacion/2,
          total_de_Azulejos/5,
          centro/1,
          loseta/2,
          tapa/5,
          cantidad_filas_completadas/2,
          jugadores_ganadores/1.

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


/*Inicio del juego
segun la cantidad de jugadores, la cantidad de losetas de fabricas que tocan
Inicio_Simulacion_Jugadores_Losetas(cantidad_jugadores,cantidad_losetas_de_fabricas)*/
inicio_Simulacion_Jugadores_Losetas(2,5).
inicio_Simulacion_Jugadores_Losetas(3,7).
inicio_Simulacion_Jugadores_Losetas(4,9).

inicio_Simulacion(Jugadores,Losetas):-
      inicio_Simulacion_Jugadores_Losetas(Jugadores,Cantidad_Losetas),
      llenar_Losetas([Cantidad_Losetas]),/*crear un array con las losetas*/
      inicializar_centro(),
      inicializar_tapa(),
      inicializar_cantidad_filas_completadas([Jugadores]),
      asignar_Tablero_Jugador([Jugadores]), /*falta crear un array con los jugadores*/
      ficha_Jugador_Inicial(X1,Jugadores),
      desarrollo_de_la_Partida().

/*COMPLETAR*/
desarrollo_de_la_Partida():-
      fase_I(Jugadores,Jugador_Inicial,Cantidad_Losetas),
      fase_II([Jugadores]),
      fila_Muro_Completada([Jugadores],Fila_llena),
      (Fila_llena=='True',
      fin_partida(),
      jugadores_ganadores([Jugadores],Ganadores),
      asserta(jugadores_ganadores(Ganadores)));
      (fase_III()).

inicializar_centro():-
      asserta(centro([])).
inicializar_tapa():-
      asserta(tapa([],[],[],[],[])).
inicializar_cantidad_filas_completadas([J1|Resto]):-
      asserta(cantidad_filas_completadas(J1,0)),
      inicializar_cantidad_filas_completadas(Resto).

fase_I(Jugadores,Jugador_Inicial,Cantidad_Losetas):-
      seleccion_Loseta(Jugador_Inicial,Cantidad_Losetas,Loseta),
      actualizar_Loseta_Tablero_Centro(Jugador_Inicial,Loseta),
      fase_I().

/*COMPLETARRRRRRR IMPORTANTE*/
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

/*COMPLETARRRRRRR IMPORTANTE*/
pasar_Azulejo_tablero(X,Jugador,Y).

fase_II([J1|Resto]):-
      espacio_preparacion(J1,[F1],[F2|Resto_F2],[F3|Resto_F3],[F4|Resto_F4],[F5|Resto_F5]),
      ((F1 =\= [],
      Color = F1,
      Fila_a_modificar=0);
      (fila_completa_espacio_preparacion([F2|Resto_F2],2,"True"),
      Color = F2,
      Fila_a_modificar=1);
      (fila_completa_espacio_preparacion([F3|Resto_F3],3,"True"),
      Color = F3,
      Fila_a_modificar=2);
      (fila_completa_espacio_preparacion([F4|Resto_F4],4,"True"),
      Color = F4,
      Fila_a_modificar=3);
      (fila_completa_espacio_preparacion([F5|Resto_F5],5,"True"),
      Color = F5,
      Fila_a_modificar=4)),

      ubicar_Azulejo_Muro(Color,J1,Fila_a_modificar,Pos),
      ubicar_resto_azulejos_en_la_tapa(Color,J1,Fila_a_modificar,Fila_a_modificar),
      actualizar_puntuacion_muro(J1,Fila_a_modificar,Pos),
      fase_II(Resto).


ubicar_resto_azulejos_en_la_tapa(Color,Jugador,0,0):-!.
ubicar_resto_azulejos_en_la_tapa(Color,Jugador,Fila_espacio_preparacion,Cant_azulejos):-
      tapa(Cant_Azulejos_Rojos,Cant_Azulejos_Amarillos,Cant_Azulejos_Blancos,Cant_Azulejos_Negros,Cant_Azulejos_Azules),
      retract(tapa(Cant_Azulejos_Rojos,Cant_Azulejos_Amarillos,Cant_Azulejos_Blancos,Cant_Azulejos_Negros,Cant_Azulejos_Azules)),
      ((Color=="rojo",
      Cant_Azulejos_Rojos_Actualizado is Cant_Azulejos_Rojos+Cant_azulejos,
      asserta(tapa(Cant_Azulejos_Rojos_Actualizado,Cant_Azulejos_Amarillos,Cant_Azulejos_Blancos,Cant_Azulejos_Negros,Cant_Azulejos_Azules)));
      (Color=="amarillo",
      Cant_Azulejos_Amarillos_Actualizado is Cant_Azulejos_Amarillos+Cant_azulejos,
      asserta(tapa(Cant_Azulejos_Rojos,Cant_Azulejos_Amarillos_Actualizado,Cant_Azulejos_Blancos,Cant_Azulejos_Negros,Cant_Azulejos_Azules)));
      (Color=="blanco",
      Cant_Azulejos_Blancos_Actualizado is Cant_Azulejos_Blancos+Cant_azulejos,
      asserta(tapa(Cant_Azulejos_Rojos,Cant_Azulejos_Amarillos,Cant_Azulejos_Blancos_Actualizado,Cant_Azulejos_Negros,Cant_Azulejos_Azules)));
      (Color=="negro",
      Cant_Azulejos_Negros_Actualizado is Cant_Azulejos_Negros+Cant_azulejos,
      asserta(tapa(Cant_Azulejos_Rojos,Cant_Azulejos_Amarillos,Cant_Azulejos_Blancos,Cant_Azulejos_Negros_Actualizado,Cant_Azulejos_Azules)));
      (Color=="azul",
      Cant_Azulejos_Azules_Actualizado is Cant_Azulejos_Azules+Cant_azulejos,
      asserta(tapa(Cant_Azulejos_Rojos,Cant_Azulejos_Amarillos,Cant_Azulejos_Blancos,Cant_Azulejos_Negros,Cant_Azulejos_Azules_Actualizado)))),

      espacio_preparacion(Jugador,F1,F2,F3,F4,F5),
      retract(espacio_preparacion(Jugador,F1,F2,F3,F4,F5)),
      (Fila_espacio_preparacion==0,
      asserta(espacio_preparacion(Jugador,[],F2,F3,F4,F5)));
      (Fila_espacio_preparacion==1,
      asserta(espacio_preparacion(Jugador,F1,[],F3,F4,F5)));
      (Fila_espacio_preparacion==2,
      asserta(espacio_preparacion(Jugador,F1,F2,[],F4,F5)));
      (Fila_espacio_preparacion==3,
      asserta(espacio_preparacion(Jugador,F1,F2,F3,[],F5)));
      (Fila_espacio_preparacion==4,
      asserta(espacio_preparacion(Jugador,F1,F2,F3,F4,[]))).

/*posicion en el muro en el que se coloco un azulejo*/
actualizar_puntuacion_muro(Jugador,Pos_Fila,Pos_Columna):-
      muro(Jugador,Tablero,Tablero_boleano),
      nth0(Pos_Fila,Tablero_boleano,Fila),

      Pos_Columna_der is Pos_Columna+1,
      recorrer_fila_hacia_derecha(Fila,Pos_Columna_der,Puntuacion_hacia_derecha),

      Pos_Columna_izq is Pos_Columna-1,
      recorrer_fila_hacia_izquierda(Fila,Pos_Columna_izq,Puntuacion_hacia_izquierda),

      extraer_columna(Tablero_boleano,Pos_Columna,Columna),
      Pos_Fila_arr is Pos_Fila - 1,
      recorrer_fila_hacia_derecha(Columna,Pos_Fila_arr,Puntuacion_hacia_abajo),
      Pos_Fila_abajo is Pos_Fila +1,
      recorrer_fila_hacia_izquierda(Columna,Pos_Fila,Puntuacion_hacia_arriba),

      Puntuacion_por_filas = 0,
      ((Puntuacion_hacia_derecha>0;
      Puntuacion_hacia_izquierda>0),
      Puntuacion_por_filas is Puntuacion_hacia_derecha+Puntuacion_hacia_izquierda+1),

      Puntuacion_por_columnas = 0,
      ((Puntuacion_hacia_arriba>0;
      Puntuacion_hacia_abajo>0),
      Puntuacion_por_columnas is Puntuacion_hacia_abajo+Puntuacion_hacia_arriba+1),

      puntuacion(Jugador,Puntuacion),
      Puntuacion_actualizada is Puntuacion + Puntuacion_por_columnas+Puntuacion_por_filas,
      retract(puntuacion(Jugador,Puntuacion)),
      asserta(puntuacion(Jugador,Puntuacion_actualizada)).


recorrer_fila_hacia_izquierda(Fila,-1,0):-!.
recorrer_fila_hacia_izquierda(Fila,Pos_Columna,Puntuacion):-
      nth0(Pos_Columna,Fila,Elemento),
      (Elemento==0,
      Puntuacion = 0,
      !);
      (Pos_Columna_anterior is Pos_Columna - 1,
      recorrer_fila_hacia_izquierda(Fila,Pos_Columna_anterior,P1),
      Puntuacion is P1 + 1).

recorrer_fila_hacia_derecha(Fila,5,0):-!.
recorrer_fila_hacia_derecha(Fila,Pos_Columna,Puntuacion):-
      nth0(Pos_Columna,Fila,Elemento),
      (Elemento==0,
      Puntuacion = 0,
      !);
      (Pos_Columna_sgt is Pos_Columna + 1,
      recorrer_fila_hacia_derecha(Fila,Pos_Columna_sgt,P1),
      Puntuacion is P1 + 1).

extraer_columna([], _ ,[]):-!.
extraer_columna([X|Xs],Indice,[Y|Ys]):-
      nth0(Indice,X,Y),
      extraer_columna(Xs,Indice,Ys).


fila_completa_espacio_preparacion(Fila,Cant_casillas_llenas,Var_booleana):-
      length(Fila,Len),
      ((Len==Cant_casillas_llenas,
      Var_booleana = "True");
      (Var_booleana="False")).
ubicar_Azulejo_Muro(Color,Jugador, Fila_Actual,Position):-
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
      crear_fila_actualizada_muro(Fila_a_modificar,0,Position,Fila_en_Proceso,Fila_Actualizada),
      insertar_tablero_actualizado(Tablero_boleano,Fila_Actual,Fila_Actualizada,Tablero_boleano_Actualizado),
      retract(muro(Jugador,Tablero_Colores,Tablero_boleano)),
      asserta(muro(Jugador,Tablero_Colores,Tablero_boleano_Actualizado)).

insertar_tablero_actualizado(Tablero_boleano,Pos_fila_modificada,Fila_modificada,Tablero_booleano_actualizado):-
      (Pos_fila_modificada==0,
      nth0(1,Tablero_boleano,F2),
      nth0(2,Tablero_boleano,F3),
      nth0(3,Tablero_boleano,F4),
      nth0(4,Tablero_boleano,F5),
      Tablero_booleano_actualizado =[Fila_modificada,F2,F3,F4,F5]);

      (Pos_fila_modificada==1,
      nth0(0,Tablero_boleano,F1),
      nth0(2,Tablero_boleano,F3),
      nth0(3,Tablero_boleano,F4),
      nth0(4,Tablero_boleano,F5),
      Tablero_booleano_actualizado =[F1,Fila_modificada,F3,F4,F5]);

      (Pos_fila_modificada==2,
      nth0(0,Tablero_boleano,F1),
      nth0(1,Tablero_boleano,F2),
      nth0(3,Tablero_boleano,F4),
      nth0(4,Tablero_boleano,F5),
      Tablero_booleano_actualizado =[F1,F2,Fila_modificada,F4,F5]);

      (Pos_fila_modificada==3,
      nth0(0,Tablero_boleano,F1),
      nth0(1,Tablero_boleano,F2),
      nth0(2,Tablero_boleano,F3),
      nth0(4,Tablero_boleano,F5),
      Tablero_booleano_actualizado =[F1,F2,F3,Fila_modificada,F5]);

      (Pos_fila_modificada==4,
      nth0(0,Tablero_boleano,F1),
      nth0(1,Tablero_boleano,F2),
      nth0(2,Tablero_boleano,F3),
      nth0(3,Tablero_boleano,F4),
      Tablero_booleano_actualizado =[F1,F2,F3,F4,Fila_modificada]).

crear_fila_actualizada_muro(Fila,Pos_Actual,Pos_a_modificar,Fila_en_Proceso,Fila_Actualizada):-
      length(Fila, Len),
      Pos_actual == Len,
      Fila_Actualizada = Fila_en_Proceso,
      !.
crear_fila_actualizada_muro(Fila,Pos_Actual,Pos_a_modificar,Fila_en_Proceso,Fila_Actualizada):-
      ((Pos_Actual==Pos_a_modificar,
      add(1, Fila_en_Proceso, Z1));
      (nth0(Pos_Actual,Fila,Elemento),
      add(Elemento,Fila_en_Proceso,Z1))),
      Pos_Actual_sgt is Pos_Actual+1,
      crear_fila_actualizada_muro(Fila,Pos_Actual_sgt,Pos_a_modificar,Z1,R1),
      Fila_Actualizada = R1.

add(X,[],[X]).
add(X,[Y|Z],[Y|W]):-add(X,Z,W).
      
fase_III():-
      llenar_Losetas([Cantidad_Losetas]),
      suma_losetas(Cantidad_Losetas,Total),
      (Total==0,
      fin_partida());
      (desarrollo_de_la_Partida()).


suma_losetas([],0):-!.
suma_losetas([L1|Resto],Total):-
      loseta(L1,Az),
      nth0(0,Az,Az1),
      nth0(1,Az,Az2),
      nth0(2,Az,Az3),
      nth0(3,Az,Az4),
      suma_losetas(Resto,Total1),
      Total is Total1 + Az1 + Az2 + Az3 + Az4.


jugadores_ganadores([J1|Resto],Jugadores):-
      puntuacion_maxima([J1|Resto],Punt_max),
      jugadores_con_puntuacion_maxima([J1|Resto],Punt_max,[],[Jugadores_con_punt]),
      length(Jugadores_con_punt,Len),
      (Len>1,
      cantidad_filas_llenas_maxima([J1|Resto],Cant_max),
      jugadores_con_filas_llenas_maxima([J1|Resto],Cant_max,[],[Jugadores_ganadores]),
      Jugadores = Jugadores_ganadores);
      Jugadores =Jugadores_con_punt.


jugadores_con_puntuacion_maxima([],P,Z1,Z1):-!.
jugadores_con_puntuacion_maxima([J1|Resto],Punt_max,Lista_en_proceso,Jugadores):-
      puntuacion(J1,Puntuacion_jugador_actual),
      (Puntuacion_jugador_actual==Punt_max,
      add(J1,Lista_en_proceso,Lista_en_proceso_act),
      jugadores_con_puntuacion_maxima(Resto,Punt_max,Lista_en_proceso_act,Jugadores_act),
      Jugadores = [Jugadores_act]).

puntuacion_maxima([],0):-!.
puntuacion_maxima([J1|Resto],Punt_max):-
      puntuacion(J1,Puntuacion_jugador_actual),
      puntuacion_maxima(Resto,Punt_max_sgt),
      (Puntuacion_jugador_actual>Punt_max_sgt,
      Punt_max = Puntuacion_jugador_actual);
      (Punt_max=Punt_max_sgt).

cantidad_filas_llenas_maxima([],0):-!.
cantidad_filas_llenas_maxima([J1|Resto],Cant_max):-
      cantidad_filas_completadas(J1,Cant_filas_jugador_actual),
      cantidad_filas_llenas_maxima(Resto,Cant_max_sgt),
      (Cant_filas_jugador_actual>Cant_max_sgt,
      Cant_max = Cant_filas_jugador_actual);
      (Cant_max=Cant_max_sgtt).

jugadores_con_filas_llenas_maxima([],P,Z1,Z1):-!.
jugadores_con_filas_llenas_maxima([J1|Resto],Filas_llenas_max,Lista_en_proceso,Jugadores):-
      cantidad_filas_completadas(J1,Cant_filas_jugador_actual),
      (Cant_filas_jugador_actual==Filas_llenas_max,
      add(J1,Lista_en_proceso,Lista_en_proceso_act),
      jugadores_con_filas_llenas_maxima(Resto,Filas_llenas_max,Lista_en_proceso_act,Jugadores_act),
      Jugadores = [Jugadores_act]).


fin_partida([J1|Resto]):-
      muro(J1,Tablero,Tablero_boleano),
      Puntuacion_por_filas=0,
      Puntuacion_por_columnas=0,
      Puntuacion_por_color_azulejo=0,

      /*Puntuacion por las columnas*/
      extraer_columna(Tablero_boleano,0,Colum_0),
      extraer_columna(Tablero_boleano,1,Colum_1),
      extraer_columna(Tablero_boleano,2,Colum_2),
      extraer_columna(Tablero_boleano,3,Colum_3),
      extraer_columna(Tablero_boleano,4,Colum_4),
      (fila_o_columna_Completa(Colum_0,0,'True'),
      Puntuacion_por_columnas is Puntuacion_por_columnas+7),
      (fila_o_columna_Completa(Colum_1,1,'True'),
      Puntuacion_por_columnas is Puntuacion_por_columnas+7),
      (fila_o_columna_Completa(Colum_2,2,'True'),
      Puntuacion_por_columnas is Puntuacion_por_columnas+7),
      (fila_o_columna_Completa(Colum_3,3,'True'),
      Puntuacion_por_columnas is Puntuacion_por_columnas+7),
      (fila_o_columna_Completa(Colum_4,4,'True'),
      Puntuacion_por_columnas is Puntuacion_por_columnas+7),


      /*Puntuacion por las filas*/
      Cant_filas_comp=0,
      nth0(0,Tablero_boleano,Fila_0),
      nth0(1,Tablero_boleano,Fila_1),
      nth0(2,Tablero_boleano,Fila_2),
      nth0(3,Tablero_boleano,Fila_3),
      nth0(4,Tablero_boleano,Fila_4),
      (fila_o_columna_Completa(Fila_0,0,'True'),
      Puntuacion_por_filas is Puntuacion_por_filas+2,
      Cant_filas_comp is Cant_filas_comp+1),
      (fila_o_columna_Completa(Fila_1,1,'True'),
      Puntuacion_por_filas is Puntuacion_por_filas+2,
      Cant_filas_comp is Cant_filas_comp+1),
      (fila_o_columna_Completa(Fila_2,2,'True'),
      Puntuacion_por_filas is Puntuacion_por_filas+2,
      Cant_filas_comp is Cant_filas_comp+1),
      (fila_o_columna_Completa(Fila_3,3,'True'),
      Puntuacion_por_filas is Puntuacion_por_filas+2,
      Cant_filas_comp is Cant_filas_comp+1),
      (fila_o_columna_Completa(Fila_4,4,'True'),
      Puntuacion_por_filas is Puntuacion_por_filas+2,
      Cant_filas_comp is Cant_filas_comp+1),

      /*Puntuacion por color azulejo*/
      /*Azulejo Azul*/
      (azulejo_color_en_Muro(0,0,Fila_0,'True'),
      azulejo_color_en_Muro(1,1,Fila_1,'True'),
      azulejo_color_en_Muro(2,2,Fila_2,'True'),
      azulejo_color_en_Muro(3,3,Fila_3,'True'),
      azulejo_color_en_Muro(4,4,Fila_4,'True'),
      Puntuacion_por_color_azulejo is Puntuacion_por_color_azulejo+10),
      /*Azulejo Rojo*/
      (azulejo_color_en_Muro(0,2,Fila_0,'True'),
      azulejo_color_en_Muro(1,3,Fila_1,'True'),
      azulejo_color_en_Muro(2,4,Fila_2,'True'),
      azulejo_color_en_Muro(3,0,Fila_3,'True'),
      azulejo_color_en_Muro(4,1,Fila_4,'True'),
      Puntuacion_por_color_azulejo is Puntuacion_por_color_azulejo+10),
      /*Azulejo Amarillo*/
      (azulejo_color_en_Muro(0,1,Fila_0,'True'),
      azulejo_color_en_Muro(1,2,Fila_1,'True'),
      azulejo_color_en_Muro(2,3,Fila_2,'True'),
      azulejo_color_en_Muro(3,4,Fila_3,'True'),
      azulejo_color_en_Muro(4,0,Fila_4,'True'),
      Puntuacion_por_color_azulejo is Puntuacion_por_color_azulejo+10),
      /*Azulejo Blanco*/
      (azulejo_color_en_Muro(0,4,Fila_0,'True'),
      azulejo_color_en_Muro(1,0,Fila_1,'True'),
      azulejo_color_en_Muro(2,1,Fila_2,'True'),
      azulejo_color_en_Muro(3,2,Fila_3,'True'),
      azulejo_color_en_Muro(4,3,Fila_4,'True'),
      Puntuacion_por_color_azulejo is Puntuacion_por_color_azulejo+10),
      /*Azulejo Negro*/
      (azulejo_color_en_Muro(0,3,Fila_0,'True'),
      azulejo_color_en_Muro(1,4,Fila_1,'True'),
      azulejo_color_en_Muro(2,0,Fila_2,'True'),
      azulejo_color_en_Muro(3,1,Fila_3,'True'),
      azulejo_color_en_Muro(4,2,Fila_4,'True'),
      Puntuacion_por_color_azulejo is Puntuacion_por_color_azulejo+10)

      cantidad_filas_completadas(J1,Cant),
      retract(cantidad_filas_completadas(J1,Cant)),
      asserta(cantidad_filas_completadas(J1,Cant_filas_comp)),

      puntuacion(J1,Puntuacion),
      Puntuacion_actualizada is Puntuacion + Puntuacion_por_color_azulejo+Puntuacion_por_columnas+Puntuacion_por_filas,
      retract(puntuacion(J1,Puntuacion)),
      asserta(puntuacion(J1,Puntuacion_actualizada)),
      fin_partida(Resto).


azulejo_color_en_Muro(Pos_fila,Pos_columna,Fila,Casilla_llena):-
      (Pos_fila==0,
      nth0(Pos_columna,Fila,Elem),
      Elem==1,
      Casilla_llena='True');

      (Pos_fila==1,
      nth0(Pos_columna,Fila,Elem),
      Elem==1,
      Casilla_llena='True');

      (Pos_fila==2,
      nth0(Pos_columna,Fila,Elem),
      Elem==1,
      Casilla_llena='True');

      (Pos_fila==3,
      nth0(Pos_columna,Fila,Elem),
      Elem==1,
      Casilla_llena='True');

      (Pos_fila==4,
      nth0(Pos_columna,Fila,Elem),
      Elem==1,
      Casilla_llena='True');

      Casilla_llena='False'.


fila_o_columna_Completa(Fila,Pos,Fila_o_columna_completa):-
      nth0(0,Fila,Elem_pos_0),
      nth0(1,Fila,Elem_pos_1),
      nth0(2,Fila,Elem_pos_2),
      nth0(3,Fila,Elem_pos_3),
      nth0(4,Fila,Elem_pos_4),

      (Elem_pos_0==1,
      Elem_pos_1==1,
      Elem_pos_2==1,
      Elem_pos_3==1,
      Elem_pos_4==1,
      Fila_o_columna_completa='True',
      !);
      (Fila_o_columna_completa='False').

fila_Muro_Completada([J1|Resto],Fila_llena_general):-
      muro(J1,Tablero,Tablero_boleano),
      filas_completa_Muro(Tablero_boleano,0,Fila_llena_bool),
      (Fila_llena_bool=='True',
      Fila_llena_general='True',!);
      (filas_Muro_Completada(Resto,Fila_llena_general1),
      Fila_llena_general=Fila_llena_general1).

filas_completa_Muro(_,5,'False'):-!.
filas_completa_Muro(Tablero,Pos_fila_actual,Fila_actual_llena):-
      nth0(Pos_fila_actual,Tablero,Fila),
      fila_o_columna_Completa(Fila,Pos_fila_actual,Fila_actual_llena),

      (Fila_actual_llena='True',
      !);
      Pos_fila_sgt is Pos_fila_actual +1,
      fila_completa_Muro(Tablero,Pos_fila_sgt,Fila_llena_sgt),
      Fila_actual_llena = Fila_llena_sgt.

ficha_Jugador_Inicial(X,Jugadores):-
      random(1,5,X),
      asserta(Jugador_Inicial,X).

llenar_Losetas([]):-!.
llenar_Losetas([Loseta|Resto]):-
      asignacion_Azulejos_Loseta(W),
      asserta(loseta(Loseta,W)),
      llenar_Losetas(Resto).

push(X,Y,[X|Y]).

asignacion_Azulejos_Loseta(Loseta_Llena):-
      
      ((asignar_azulejo_Loseta(Y1,Bolsa_y_tapa_vacia1),
      Bolsa_y_tapa_vacia1=='False');
      (Y1=0)),

      ((asignar_azulejo_Loseta(Y2,Bolsa_y_tapa_vacia2),
      Bolsa_y_tapa_vacia2=='False');
      (Y2=0)),

      ((asignar_azulejo_Loseta(Y3,Bolsa_y_tapa_vacia3),
      Bolsa_y_tapa_vacia3=='False');
      (Y3=0)),

      ((asignar_azulejo_Loseta(Y4,Bolsa_y_tapa_vacia4),
      Bolsa_y_tapa_vacia4=='False');
      (Y4=0)),
      ((Y1=\=0,
      asignar_color(Y1,X1));
      (X1=0)),

      ((Y2=\=0,
      asignar_color(Y2,X2));
      (X2=0)),

      ((Y3=\=0,
      asignar_color(Y3,X3));
      (X3=0)),

      ((Y4=\=0,
      asignar_color(Y4,X4));
      (X4=0)),

      Loseta_Llena=[X1,X2,X3,X4].

      /*random(1,5,Y1),
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
      Loseta_Llena=[X1,X2,X3,X4].*/

asignar_azulejo_Loseta(Azulejo,Game_over):-
      (azulejos_en_la_bolsa('True'),
      buscar_Azulejo_en_la_bolsa(Y),
      actualizar_cantidad_Azulejos(Y1),
      Azulejo=Y,
      Game_over='False');
      (rellenar_azulejos_tapa_bolsa('True'),
      buscar_Azulejo_en_la_bolsa(Y),
      actualizar_cantidad_Azulejos(Y1),
      Azulejo=Y,
      Game_over='False');
      Game_over='True'.

rellenar_azulejos_tapa_bolsa(Tapa_vacia):-
      tapa(R,Am,B,N,Az),
      Total is R+Am+B+N+Az,
      (Total==0,
      Tapa_vacia='True',
      !);
      (total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,Azules),
      Rojos_Act is Rojos+R,
      Amarillos_Act is Amarillos+Am,
      Blancos_Act is Blancos+B,
      Negros_Act is Negros+N,
      Azules_Act is Azules+Az,
      retract(tapa(R,Am,B,N,Az)),
      asserta(tapa(0,0,0,0,0)),
      retract(total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,Azules)),
      asserta(total_de_Azulejos(Rojos_Act,Amarillos_Act,Blancos_Act,Negros_Act,Azules_Act)),
      Tapa_vacia='False').

azulejos_en_la_bolsa(Bolsa_vacia):-
      total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,Azules)
      Total is Rojos+Amarillos+Blancos+Negros+Azules,
      (Total==0,
      Bolsa_vacia='True',!);
      Bolsa_vacia='False'.


buscar_Azulejo_en_la_bolsa(Azulejo):-
      total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,Azules),
      random(1,5,Color),
      Cant_Azulejos =[Rojos,Amarillos,Blancos,Negros,Azules],
      Pos is Color -1,
      nth0(Pos,Cant_Azulejos,Cant),
      ((Cant>=0,
      Azulejo = Color);
      (buscar_Azulejo_en_la_bolsa(Azulejo))).


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


