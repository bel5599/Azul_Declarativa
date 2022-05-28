

:-dynamic muro/3,
          espacio_preparacion/6,
          suelo/2,
          puntuacion/2,
          total_de_Azulejos/5,
          centro/1,
          loseta/2,
          tapa/5,
          cantidad_filas_completadas/2,
          jugadores_ganadores/1,
          color_existe_en_esta_loseta/3,
          relacion_loseta_jugador_color_actual/6,
          ficha_Jugador_Inicial/1.

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

inicio_Simulacion(Cant_Jugadores):-
      inicio_Simulacion_Jugadores_Losetas(Cant_Jugadores,Cantidad_Losetas),
      Len is Cant_Jugadores+1,
      random(0,Len,Jugador_Inicial),
      asserta(ficha_Jugador_Inicial(Jugador_Inicial)),
      crear_lista_Jugadores(Jugador_Inicial,1,Cant_Jugadores,[],[Jugadores]),
      crear_Lista_Losetas(0,Cantidad_Losetas,[],[Losetas]),
      llenar_Losetas(Losetas),/*crear un array con las losetas*/
      inicializar_centro(),
      inicializar_tapa(),
      inicializar_cantidad_filas_completadas(Jugadores),
      asignar_Tablero_Jugador(Jugadores), /*falta crear un array con los jugadores*/
      
      desarrollo_de_la_Partida(Jugadores,Losetas).


desarrollo_de_la_Partida(Jugadores,Losetas):-
      centro(W),
      add(-1,W,W_actualizado),
      retract(centro(W)),
      asserta(centro(W_actualizado)),

      fase_I(Jugadores,Losetas),

      fase_II(Jugadores),
      fila_Muro_Completada(Jugadores,Fila_llena),
      (Fila_llena=='True',
      fin_partida(),
      jugadores_ganadores(Jugadores,Ganadores),
      asserta(jugadores_ganadores(Ganadores)));
      (fase_III()).

inicializar_centro():-
      asserta(centro([])).
inicializar_tapa():-
      asserta(tapa([],[],[],[],[])).
inicializar_cantidad_filas_completadas([J1|Resto]):-
      asserta(cantidad_filas_completadas(J1,0)),
      inicializar_cantidad_filas_completadas(Resto).
crear_lista_Jugadores(Jugador_Inicial,Jugador_Inicial,Cant_Jugadores,Lista_en_proceso,Lista_resultante):-
      length(Lista_en_proceso,Len),
      Len>0,
      Lista_resultante=Lista_en_proceso,!.
crear_lista_Jugadores(Jugador_Inicial,Pos_actual,Cant_Jugadores,Lista_en_proceso,Lista_resultante):-
      (Pos_actual=:=Cant_Jugadores+1,
      Pos_actual=1),
      add(Pos_actual,Lista_en_proceso,Lista_en_proceso_act),
      Pos_actual_sgt is Pos_actual+1,
      crear_lista_Jugadores(Jugador_Inicial,Pos_actual_sgt,Cant_Jugadores,Lista_en_proceso_act,Lista_resultante_sgt),
      Lista_resultante=Lista_resultante_sgt.


fase_I([],Cantidad_Losetas):-!.

fase_I([Jugador_Inicial|Resto],Losetas):-
      length(Losetas,Cantidad_Losetas),
      seleccion_Loseta(Jugador_Inicial,Losetas,Loseta),
      (Loseta==Cantidad_Losetas,
      centro(W),
      actualizar_Loseta_Tablero_Centro(Jugador_Inicial,Loseta,'True'),

      se_encuentra_ficha_jugador_inicial(W,'True'),
      ficha_Jugador_Inicial(J1),
      retract(ficha_Jugador_Inicial(J1)),
      asserta(ficha_Jugador_Inicial(Jugador_Inicial)).
      actualizar_muro(-1));

      (actualizar_Loseta_Tablero_Centro(Jugador_Inicial,Loseta,'False');
      loseta(Loseta,W),
      retract(loseta(Loseta,W)),
      asserta(loseta(Loseta,[]))),
      fase_I(Resto,Losetas).

actualizar_muro(Color):-
      centro(W)
      eliminar_azulejo_centro(W,Color,[],W_actualizado),
      retract(centro(W)),
      asserta(centro(W_actualizado)).
eliminar_azulejo_centro([],Color,Lista_en_proceso,Lista_en_proceso):-!.
eliminar_azulejo_centro([Azulejo|Resto],Color,Lista_en_proceso,Lista_resultante):-
      (Azulejo==Color,
      eliminar_azulejo_centro(Resto,Color,Lista_en_proceso,Lista_resultante_sgt),
      Lista_resultante = Lista_resultante_sgt);
      add(Azulejo,Lista_en_proceso,Lista_en_proceso_act),
      eliminar_azulejo_centro(Resto,Color,Lista_en_proceso_act,Lista_resultante_sgt),
      Lista_resultante = Lista_resultante_sgt.


se_encuentra_ficha_jugador_inicial([],'False'):-!.
se_encuentra_ficha_jugador_inicial([Azulejo|Resto],Se_encuentra):-
      (Azulejo==-1,
      Se_encuentra='True');
      se_encuentra_ficha_jugador_inicial(Resto,Se_encuentra_sgt),
      Se_encuentra=Se_encuentra_sgt.


crear_Lista_Losetas(Pos_Actual,Cant_Losetas,Lista_actual,Lista_Losetas):-
      Pos_Actual=:=Cant_Losetas-1,
      centro(W),
      length(W,Len),
      (Len==1,
      nth0(0,W,-1));
      (Len>0,
      add(Pos_Actual,Lista_actual,Lista_actual_actualizada),
      Lista_Losetas=Lista_actual_actualizada);
      Lista_Losetas=Lista_actual.
crear_Lista_Losetas(Pos_Actual,Cant_Losetas,Lista_actual,Lista_Losetas):-
      add(Pos_Actual,Lista_actual,Lista_actual_actualizada),
      Pos_Actual_sgt is Pos_Actual+1,
      crear_Lista_Losetas(Pos_Actual_sgt,Cant_Losetas,Lista_actual_actualizada,Lista_Losetas_sgt),
      Lista_Losetas=Lista_Losetas_sgt.


/*COMPLETARRRRRRR IMPORTANTE YA ESTA COMPLETO ESTE METODO SOLO QUEDA PROBARLO COMO TODO LO DEMAS :)*/ 
seleccion_Loseta(Jugador,Losetas,Loseta):-
      losetas_con_menor_penalizacion(Jugador,[Losetas],[Losetas_optimas]),
      length(Losetas_optimas,Len),
      random(0,Len,Pos),
      nth0(Pos,Losetas_optimas,Loseta_seleccionada),
      Loseta = Loseta_seleccionada.

losetas_con_menor_penalizacion(Jugador,[L1|Resto],Losetas):-
      losetas_que_completan_filas(Jugador,[L1|Resto],[],Losetas_que_completan_filas),
      extraer_columna(Losetas_que_completan_filas,1,Penalizacion_por_Loseta),
      penalizacion_minima(Penalizacion_por_Loseta,Penalizacion_minima),
      losetas_optimas(Losetas_que_completan_filas,Penalizacion_minima,[],Losetas_optimas),
      Losetas=Losetas_optimas.
      

losetas_optimas([],Penalizacion_minima,Losetas_optimas_en_proceso,Losetas_optimas_en_proceso):-!.
losetas_optimas([L1|Resto],Penalizacion_minima,Losetas_optimas_en_proceso,Losetas_optimas):-
      nth0(1,L1,Penalizacion_loseta_actual),
      nth0(0,L1,Loseta_actual),
      (Penalizacion_loseta_actual==Penalizacion_minima,
      add(Loseta_actual,Losetas_optimas_en_proceso,Losetas_optimas_en_proceso_atualizada),
      losetas_optimas(Resto,Penalizacion_minima,Losetas_en_proceso_actualizada,Losetas_optimas_actulizada),
      Losetas_optimas=Losetas_optimas_actulizada);
      (losetas_optimas(Resto,Penalizacion_minima,Losetas_en_proceso,Losetas_optimas_actulizada),
      Losetas_optimas=Losetas_optimas_actulizada).


penalizacion_minima([],100):-!.
penalizacion_minima([L1|Resto],Penalizacion_minima):-
      penalizacion_minima(Resto,Penalizacion_minima_sgt),
      (Penalizacion_minima_sgt<L1,
      penalizacion_minima = Penalizacion_minima_sgt);
      (penalizacion_minima=L1).


losetas_que_completan_filas(Jugador,[],Losetas_en_proceso,Losetas_en_proceso):-!.
/*losetas_que_completan_filas(Jugador,[X],Losetas_en_proceso,Losetas_optimas):-
      centro(W),
      se_encuentra_ficha_jugador_inicial(W,'True'),
      ficha_Jugador_Inicial(J1),
      retract(ficha_Jugador_Inicial(J1)),
      asserta(ficha_Jugador_Inicial()).*/

losetas_que_completan_filas(Jugador,[L1|Resto],Losetas_en_proceso,Losetas_optimas):-
      cant_colores_loseta(L1,Cant_apariciones_de_color),
      completar_fila_zona_preparacion_loseta(L1,Cant_apariciones_de_color,Jugador,Completa_fila,Penalizacion),
      (Completa_fila=='True',
      add([L1,Penalizacion],Losetas_en_proceso,Losetas_en_proceso_actualizada),
      losetas_que_completan_filas(Jugador,Resto,Losetas_en_proceso_actualizada,Losetas_optimas_actualizada),
      Losetas_optimas=Losetas_optimas_actualizada);
      (losetas_que_completan_filas(Jugador,Resto,Losetas_en_proceso,Losetas_optimas_actualizada),
      Losetas_optimas=Losetas_optimas_actualizada).

cant_colores_loseta(L1,Es_el_centro,[Cant_apariciones_de_color]):-
      (Es_el_centro,
      centro(W));
      (loseta(L1,W)),
      length(W,Len),
      cant_colores_loseta_recursivo(L1,W,0,Len,[],Cant_apariciones),
      Cant_apariciones_de_cada_color = Cant_apariciones,
      actualizar_base_datos(L1,0,Len).
/*codigo_basura():-
      loseta(L1,colores),
      cant_azulejos_color_item0 =0,
      nth0(0,colores,Item_0),

      (Item_0==0,termine!);
      cant_azulejos_color_item0=1,
      asserta(color_existe_en_esta_loseta(L1,Item_0,'True',1)),
      nth0(1,colores,Item_1),

      ((Item_1==Item_0,
      color_existe_en_esta_loseta(L1,Item_0,'True',X1),
      retract(color_existe_en_esta_loseta(L1,Item_0,'True',X1)),
      X2 is X1+1,
      asserta(color_existe_en_esta_loseta(L1,Item_0,'True',X2)),
      asserta(color_existe_en_esta_loseta(L1,Item_1,'False',0)));

      (asserta(color_existe_en_esta_loseta(L1,Item_1,'True',1)))),

      nth0(2,colores,Item_2),

      ((Item_2==Item_0,
      color_existe_en_esta_loseta(L1,Item_0,'True',Y1),
      retract(color_existe_en_esta_loseta(L1,Item_0,'True',Y1)),
      Y2 is Y1+1,
      asserta(color_existe_en_esta_loseta(L1,Item_0,'True',Y2)),
      asserta(color_existe_en_esta_loseta(L1,Item_2,'False',0)));

      (Item_2==Item_1,
      color_existe_en_esta_loseta(L1,Item_1,'True',Z1),
      Z2 is Z1+1,
      retract(color_existe_en_esta_loseta(L1,Item_1,'True',Z1)),
      asserta(color_existe_en_esta_loseta(L1,Item_1,'True',Z2)),
      asserta(color_existe_en_esta_loseta(L1,Item_2,'False',0)));

      asserta(color_existe_en_esta_loseta(L1,Item_2,'True',1))),
      nth0(3,colores,Item_3),

      ((Item_3==Item_0,
      color_existe_en_esta_loseta(L1,Item_0,'True',W1),
      W2 is W1+1,
      retract(color_existe_en_esta_loseta(L1,Item_0,'True',W1)),
      asserta(color_existe_en_esta_loseta(L1,Item_0,'True',W2)),
      asserta(color_existe_en_esta_loseta(L1,Item_3,'False',0)));

      (Item_3==Item_1,
      color_existe_en_esta_loseta(L1,Item_1,'True',A1),
      A2 is A1+1,
      retract(color_existe_en_esta_loseta(L1,Item_1,'True',A1)),
      asserta(color_existe_en_esta_loseta(L1,Item_1,'True',A2)),
      asserta(color_existe_en_esta_loseta(L1,Item_3,'False',0)));

      (Item_3==Item_2,
      color_existe_en_esta_loseta(L1,Item_2,'True',B1),
      B2 is B1+1,
      retract(color_existe_en_esta_loseta(L1,Item_2,'True',B1)),
      asserta(color_existe_en_esta_loseta(L1,Item_2,'True',B2)),
      asserta(color_existe_en_esta_loseta(L1,Item_3,'False',0)));

      asserta(color_existe_en_esta_loseta(L1,Item_3,'True',1))),
      
      color_existe_en_esta_loseta(L1,Item_0,Var_item0,Cant_apariciones_item_0),
      color_existe_en_esta_loseta(L1,Item_1,Var_item1,Cant_apariciones_item_1),
      color_existe_en_esta_loseta(L1,Item_2,Var_item2,Cant_apariciones_item_2),
      color_existe_en_esta_loseta(L1,Item_3,Var_item3,Cant_apariciones_item_3),
      Cant_apariciones_de_color=[Cant_apariciones_item_0,Cant_apariciones_item_1,Cant_apariciones_item_2,Cant_apariciones_item_3],
      
      retract(color_existe_en_esta_loseta(L1,Item_0,Var_item0,Cant_apariciones_item_0)),
      retract(color_existe_en_esta_loseta(L1,Item_1,Var_item1,Cant_apariciones_item_1)),
      retract(color_existe_en_esta_loseta(L1,Item_2,Var_item2,Cant_apariciones_item_2)),
      retract(color_existe_en_esta_loseta(L1,Item_3,Var_item3,Cant_apariciones_item_3)).*/

actualizar_base_datos(L1,W,Pos_actual,Total_Azulejos):-
      (Pos_actual=:=Total_Azulejos-1,
      /*loseta(L1,W),*/
      nth0(Pos_actual,W,Color),
      color_existe_en_esta_loseta(L1,Color,Var_item,Cant_apariciones_item),
      retract(color_existe_en_esta_loseta(L1,Color,Var_item,Cant_apariciones_item))).

actualizar_base_datos(L1,W,Pos_actual,Total_Azulejos):-
      /*loseta(L1,W),*/
      nth0(Pos_actual,W,Color),
      color_existe_en_esta_loseta(L1,Color,Var_item,Cant_apariciones_item),
      retract(color_existe_en_esta_loseta(L1,W,Color,Var_item,Cant_apariciones_item)),
      Pos_Actual_sgt is Pos_actual+1,
      actualizar_base_datos(L1,W,Pos_Actual_sgt,Total_Azulejos).

cant_colores_loseta_recursivo(L1,W,Pos_actual,Cant_Azulejos,Lista_en_proceso,Cant_apariciones):-
      (Pos_actual=:=Cant_Azulejos-1,
      Cant_apariciones=Lista_en_proceso),!.
cant_colores_loseta_recursivo(L1,W,Pos_actual,Cant_azulejos_loseta_Actual,Lista_en_proceso,Cant_apariciones_de_cada_color):-
      /*loseta(L1,W),*/
      nth0(Pos_actual,W,Color),
      color_actual_igual_a_otro_ya_existente(0,Pos_actual,Color,L1,W),
      color_existe_en_esta_loseta(L1,W,Item,Var_item,Cant_apariciones_item),
      add(Cant_apariciones,Lista_en_proceso,Lista_en_proceso_act),
      Pos_Actual_sgt is Pos_actual+1,
      cant_colores_loseta_recursivo(L1,W,Pos_Actual_sgt,Cant_azulejos_loseta_Actual,Lista_en_proceso_act,Cant_apariciones_de_cada_color_sgt),
      Cant_apariciones_de_cada_color = Cant_apariciones_de_cada_color_sgt.

color_actual_igual_a_otro_ya_existente(0,0,Color,L1,W):-
      /*loseta(L1,W),*/
      nth0(0,W,Item),
      (Item==0,/*termine*/!);
      asserta(color_existe_en_esta_loseta(L1,Item,'True',1)).

color_actual_igual_a_otro_ya_existente(Pos_color_actual,Pos_color_actual,Color,L1,W):-
      /*loseta(L1,W),*/
      nth0(Pos_color_actual,W,Item),
      asserta(color_existe_en_esta_loseta(L1,Item,'True',1)),!.

color_actual_igual_a_otro_ya_existente(Pos_inicial,Pos_color_actual,Color,L1,W):-
      /*loseta(L1,W),*/
      nth0(Pos_inicial,W,Item),
      (Item==Color,
      color_existe_en_esta_loseta(L1,Item,'True',X1),
      retract(color_existe_en_esta_loseta(L1,Item,'True',X1)),
      X2 is X1+1,
      asserta(color_existe_en_esta_loseta(L1,Item,'True',X2)),
      asserta(color_existe_en_esta_loseta(L1,Color,'False',0)));
      (Pos_inicial_sgt is Pos_inicial+1,
      color_actual_igual_a_otro_ya_existente(Pos_inicial_sgt,Pos_color_actual,Color,L1)).

completar_fila_zona_preparacion_loseta(L1,Cant_apariciones_de_color,J1,Completa_fila,Penalizacion):-
      loseta(L1,Colores),
      completar_fila_zona_preparacion_loseta_recursivo(L1,Cant_apariciones_de_color,0,J1,Completa_fila_actual,0,Penalizacion_actual),
      Completa_fila=Completa_fila_actual,
      Penalizacion=Penalizacion_actual.

/*codigo_basura():-
      nth0(0,Colores,Item_0),
      nth0(0,Cant_apariciones_de_color,Cant_Item_0)
      completar_fila_zona_preparacion_loseta_cada_color(Item_0,Cant_Item_0,Completa_fila_Item_0,Penalizacion_Item_0),

      nth0(1,Colores,Item_1),
      nth0(1,Cant_apariciones_de_color,Cant_Item_1)
      completar_fila_zona_preparacion_loseta_cada_color(Item_1,Cant_Item_1,Completa_fila_Item_1,Penalizacion_Item_1),

      nth0(2,Colores,Item_2),
      nth0(2,Cant_apariciones_de_color,Cant_Item_2)
      completar_fila_zona_preparacion_loseta_cada_color(Item_2,Cant_Item_2,Completa_fila_Item_2,Penalizacion_Item_2),

      nth0(3,Colores,Item_3),
      nth0(3,Cant_apariciones_de_color,Cant_Item_3)
      completar_fila_zona_preparacion_loseta_cada_color(Item_3,Cant_Item_3,Completa_fila_Item_3,Penalizacion_Item_3),

      (((Completa_fila_Item_0=='True',
      asserta(relacion_loseta_jugador_color_actual(J1,L1,Item_0,Cant_item_0,Completa_fila_Item_0,Penalizacion_Item_0)));
      (Completa_fila_Item_1=='True',
      asserta(relacion_loseta_jugador_color_actual(J1,L1,Item_1,Cant_item_1,Completa_fila_Item_1,Penalizacion_Item_1)),
      asserta(relacion_loseta_jugador_color_actual(J1,L1,Item_0,Cant_item_0,Completa_fila_Item_0,Penalizacion_Item_0)));
      (Completa_fila_Item_2=='True',
      asserta(relacion_loseta_jugador_color_actual(J1,L1,Item_2,Cant_item_2,Completa_fila_Item_2,Penalizacion_Item_2)),
      asserta(relacion_loseta_jugador_color_actual(J1,L1,Item_0,Cant_item_0,Completa_fila_Item_0,Penalizacion_Item_0)),
      asserta(relacion_loseta_jugador_color_actual(J1,L1,Item_1,Cant_item_1,Completa_fila_Item_1,Penalizacion_Item_1)));
      (Completa_fila_Item_3=='True',
      asserta(relacion_loseta_jugador_color_actual(J1,L1,Item_3,Cant_item_3,Completa_fila_Item_3,Penalizacion_Item_3)),
      asserta(relacion_loseta_jugador_color_actual(J1,L1,Item_2,Cant_item_2,Completa_fila_Item_2,Penalizacion_Item_2)),
      asserta(relacion_loseta_jugador_color_actual(J1,L1,Item_0,Cant_item_0,Completa_fila_Item_0,Penalizacion_Item_0)),
      asserta(relacion_loseta_jugador_color_actual(J1,L1,Item_1,Cant_item_1,Completa_fila_Item_1,Penalizacion_Item_1)))),
      Completa_fila='True',

      Penalizacion is Penalizacion_Item_0+Penalizacion_Item_1+Penalizacion_Item_2+Penalizacion_Item_3);
      Completa_fila='False'.*/
completar_fila_zona_preparacion_loseta_recursivo(L1,Cant,Azulejos,Pos_actual,J1,Completa_fila,Penalizacion_actual,Penalizacion):-
      length(Azulejos,Len),
      Pos_Actual==Len,
      Completa_fila='False',
      Penalizacion=Penalizacion_actual.
completar_fila_zona_preparacion_loseta_recursivo(L1,Cant_apariciones_de_color,Azulejos_loseta,Posicion_actual,J1,Completa_fila,Penalizacion_en_proceso, Penalizacion):-
      nth0(Posicion_actual,Colores,Item),
      nth0(Posicion_actual,Cant_apariciones_de_color,Cant_Item)
      completar_fila_zona_preparacion_loseta_cada_color(Item,Cant_Item,Completa_fila_Item,Penalizacion_Item),
      asserta(relacion_loseta_jugador_color_actual(J1,L1,Item,Cant_item,Completa_fila_Item,Penalizacion_Item))

      Posicion_actual_sgt is Posicion_actual+1,
      Penalizacion_en_proceso_sgt is Penalizacion_en_proceso+Penalizacion_Item,
      completar_fila_zona_preparacion_loseta_recursivo(L1,Cant_apariciones_de_color,Azulejos_loseta,Posicion_actual_sgt,J1,Completa_fila_sgt,Penalizacion_en_proceso_sgt,Penalizacion_actualizada),
      ((Completa_fila_Item==True,
      Completa_fila=True)
      (Completa_fila= Completa_fila_sgt)),
      Penalizacion=Penalizacion_actualizada.

      
completar_fila_zona_preparacion_loseta_cada_color(Color,Cant_apariciones,Completa_fila,Penalizacion):-
      nth0(1,Cant_apariciones_de_color,Cant_Item),
      (Cant_Item==0,
      Penalizacion=0,
      Completa_fila='False');
      (completar_fila_zona_preparacion_color(Color,Cant_Item,J1,Completa_fila,Penalizacion),
      Completa_fila=='True');
      (Penalizacion=0,
      Completa_fila='False').

completar_fila_zona_preparacion_color(Color,Cant_apariciones,J1,Completa_fila,Penalizacion):-
      espacio_preparacion(J1,F1,F2,F3,F4,F5),

      (Cant_apariciones==1,
      (cant_apariciones_fila_con_penalizacion(Color,Cant_apariciones,[F1],1,'True',Penalizacion_1),
      Completa_fila=True,
      Penalizacion=Penalizacion_1);
      Completa_fila='False');

      /*CUANDO HAY 2 AZULEJOS CON EL MISMO COLOR*/
      (Cant_apariciones==2,
      (cant_apariciones_fila_con_penalizacion(Color,Cant_apariciones,[F2,F1],2,'True',Penalizacion_1),
      Completa_fila=True,
      Penalizacion=Penalizacion_1);
      Completa_fila='False');

      /*CUANDO HAY 3 AZULEJOS CON EL MISMO COLOR*/
      (Cant_apariciones==3,
      (cant_apariciones_fila_con_penalizacion(Color,Cant_apariciones,[F3,F2,F1],3,'True',Penalizacion_1),
      Completa_fila=True,
      Penalizacion=Penalizacion_1);
      Completa_fila='False');

      /*CUANDO HAY 4 AZULEJOS CON EL MISMO COLOR*/
      (Cant_apariciones==4,
      (cant_apariciones_fila_con_penalizacion(Color,Cant_apariciones,[F4,F3,F2,F1],4,'True',Penalizacion_1),
      Completa_fila=True,
      Penalizacion=Penalizacion_1);
      Completa_fila='False');

      /*CUANDO HAY 5 O MAS AZULEJOS CON EL MISMO COLOR*/
      (Cant_apariciones>=5,
      (cant_apariciones_fila_con_penalizacion(Color,Cant_apariciones,[F5,F4,F3,F2,F1],5,'True',Penalizacion_1),
      Completa_fila=True,
      Penalizacion=Penalizacion_1);
      Completa_fila='False');


cant_apariciones_fila_con_penalizacion(Color,Cant_apariciones,[F1],1,Completa_fila,Penalizacion):-
      (azulejo_valido_en_la_fila_actual(Color,Cant_apariciones,Fila_Actual,Tamano_max_fila_Actual,'True'),
      Completa_fila='True',
      Cant_apariciones>1,
      Cant_casillas_a_ocupar_en_el_suelo is Cant_apariciones-1,
      cant_puntos_negativos_en_la_posicion_actual_del_suelo(J1,Cant_casillas_a_ocupar_en_el_suelo,Puntos_negativos),
      Penalizacion=Puntos_negativos);
      Completa_fila='False'.
cant_apariciones_fila_con_penalizacion(Color,Cant_apariciones,[Fila_Actual|Resto],Tamano_max_fila_Actual,Completa_fila,Penalizacion):-
      (azulejo_valido_en_la_fila_actual(Color,Cant_apariciones,Fila_Actual,Tamano_max_fila_Actual,'True'),
      length(Fila_Actual,Len),
      Len_actualizado is Len + Cant_apariciones,

      (Len_actualizado>Tamano_max_fila_Actual,
      Cant_casillas_a_ocupar_en_el_suelo is Len_actualizado-Tamano_max_fila_Actual
      cant_puntos_negativos_en_la_posicion_actual_del_suelo(J1,Cant_casillas_a_ocupar_en_el_suelo,Puntos_negativos),
      Penalizacion= Puntos_negativos);
      (Penalizacion=0),
      Completa_fila='True');
      Tamano_max_fila_Actual_sgt is Tamano_max_fila_Actual - 1,
      cant_apariciones_fila_con_penalizacion(Color,Cant_apariciones,Resto,Tamano_max_fila_Actual_sgt,Completa_fila_sgt,Penalizacion_sgt),
      Completa_fila=Completa_fila_sgt,
      Penalizacion=Penalizacion_sgt.

cant_puntos_negativos_en_la_posicion_actual_del_suelo(J1,Casillas_a_ocupar,Valor).
/*predicado donde se comprueba si se puede ubicar un azulejo del color correspondiente
% en la posicion entrante
A- color del azulejo, [X,Y] - posicion, [MP]-matriz de preparacion,[MM]- matriz del muro, W true o false   FALTAN COSAS AQUI
azulejo_en_posicion_valida(A,[X,Y],Fila_zona_preparacion,[M1],[M2],W):-
      posicion_valida_en_zona_preparacion(X,Y,1), 
      recorrer_fila_zona_preparacion(A,Fila_zona_preparacion,1),
      recorrer_fila_muro(A,[M1],[M2],1).*/

azulejo_valido_en_la_fila_actual(Color,Cant_apariciones,Fila,Tamano_max_fila,Azulejo_valido):-
      casilla_vacias_para_ubicar_azulejos_nuevos(Cant_apariciones,Fila,Tamano_max_fila,'True'),
      fila_con_los_mismos_colores(Color,Fila,'True'),
      fila_muro_con_distintos_colores(Color,Fila,'True'),
      Azulejo_valido='True'.

casilla_vacias_para_ubicar_azulejos_nuevos(Cant_azulejos,Fila,Tamano_max_fila,Casillas_vacias_suficientes):-
      length(Fila,Len),
      (Len==Tamano_max_fila,
      Casillas_vacias_suficientes='False',!);
      Casillas_vacias_suficientes='True'.


fila_con_los_mismos_colores(Color,[],'True'):-!.
fila_con_los_mismos_colores(Color,[A1|Resto],Mismo_color):-
      (Color==A1,
      fila_con_los_mismos_colores(Color,Resto,Mismo_color_sgt),
      Mismo_color = Mismo_color_sgt);
      Mismo_color='False'.

fila_muro_con_distintos_colores(Color,[],'True'):-!.
fila_muro_con_distintos_colores(Color,[A1|Resto],Diferente_color):-
      (Color!=A1,
      fila_muro_con_distintos_colores(Color,Resto,Diferente_color_sgt),
      Diferente_color = Diferente_color_sgt);
      Diferente_color='False'.

/*verificar que en la fila donde se va a poner la ficha las demas que esten sean del mismo color
recorrer_fila_zona_preparacion(A,[],1):-!.
recorrer_fila_zona_preparacion(A,[X],0):-
      A=\=X,!.
recorrer_fila_zona_preparacion(A,[X|M],W):-
      recorrer_fila_zona_preparacion(A,[M],W1),
      W is W1.

Recorre una fila y va comparando si se encuentra el elemento
recorrer_fila_muro(X,[],[],0):-!.
recorrer_fila_muro(X,[X1|Y],[X2|Y1],W):-
      X==X1,
      X2==0,
      W=1,!;
      X==X1,
      X2==1,
      W=0,!;
      recorrer_fila_muro(X,Y,Y1,W1).*/



actualizar_Loseta_Tablero_Centro(Jugador,Loseta,Es_el_centro):-
      (Es_el_centro=='True',
      centro([Azulejo|Resto]),
      seleccion_Azulejos(Jugador,Es_el_centro,Loseta,[Azulejo|Resto]));
      (loseta(Loseta,[Azulejo|Resto]),
      seleccion_Azulejos(Jugador,Es_el_centro,Loseta,[Azulejo|Resto]),
      retract(loseta(Loseta,[Azulejo|Resto])),
      asserta(loseta(Loseta,[]))),
      retractall(relacion_loseta_jugador_color_actual).

seleccion_Azulejos(Jugador,Es_el_centro,Loseta_actual,[X|Resto]):-
      relacion_loseta_jugador_color_actual(Jugador,Loseta_actual,X,Cant_X,Completa_fila_X,Penalizacion_X),
      (Cant_X==0);
      (Completa_fila_X=='True',
      (Es_el_centro=='True',
      actualizar_muro(X,Cant_X)),
      pasar_Azulejo_tablero(Jugador,X,Cant_X,Cant_X,Cant_X,Penalizacion_X),
      annadir_azulejos_centro(Resto,Jugador,Loseta_actual));
      annadir_azulejos_centro(X),
      seleccion_Azulejos(Jugador, Loseta_actual, Resto).
      /*centro(Azulejos_Centro),
      retract(centro(Azulejos_Centro)),
      push(X,Azulejos_Centro,Azulejos_Centro_Actualizado),
      asserta(centro(Azulejos_Centro_Actualizado)),
      seleccion_Azulejos(Jugador,[Resto]))*/

annadir_azulejos_centro([A1|Resto],Jugador,Loseta_actual):-
      relacion_loseta_jugador_color_actual(Jugador,Loseta_actual,A1,Cant_A1,Completa_fila,Penalizacion),
      (Cant_A1==0,
      annadir_azulejos_centro(Resto,Jugador,Loseta_actual));
      annadir_azulejos_centro_varias_cantidades(A1,Cant_A1),
      annadir_azulejos_centro(Resto,Jugador,Loseta_actual).

annadir_azulejos_centro_varias_cantidades(A1,0):-!.
annadir_azulejos_centro_varias_cantidades(Azulejo,Cant):-
      centro(Azulejos_Centro),
      retract(centro(Azulejos_Centro)),
      push(Azulejo,Azulejos_Centro,Azulejos_Centro_Actualizado),
      asserta(centro(Azulejos_Centro_Actualizado)),
      Cant_sgt is Cant-1,
      annadir_azulejos_centro_varias_cantidades(Azulejo,Cant_sgt).

/*COMPLETARRRRRRR IMPORTANTE*/
pasar_Azulejo_tablero(Jugador,X,Cant_X,Fila,Tamanno_max_fila,Penalizacion):-
      azulejo_valido_en_la_fila_actual(X,Cant_X,Fila,Tamanno_max_fila,Azulejo_valido),
      (Azulejo_valido=='True',
      espacio_preparacion(Jugador,F1,F2,F3,F4,F5),
      retract(espacio_preparacion(Jugador,F1,F2,F3,F4,F5)),
      ((F1==Cant_X,
      add(X,F1,F1_actualizado),
      asserta(espacio_preparacion(Jugador,F1_actualizado,F2,F3,F4,F5)));
      (F2==Cant_X,
      add(X,F2,F2_actualizado),
      asserta(espacio_preparacion(Jugador,F1,F2_actualizado,F3,F4,F5)));
      (F3==Cant_X,
      add(X,F3,F3_actualizado),
      asserta(espacio_preparacion(Jugador,F1,F2,F3_actualizado,F4,F5)));
      (F4==Cant_X,
      add(X,F4,F4_actualizado),
      asserta(espacio_preparacion(Jugador,F1,F2,F3,F4_actualizado,F5)));
      (F5==Cant_X,
      add(X,F5,F5_actualizado),
      asserta(espacio_preparacion(Jugador,F1,F2,F3,F4,F5_actualizado)))),

      length(Fila,Len),
      Len_actualizado is Len + Cant_X,

      (Len_actualizado>Tamano_max_fila_Actual,
      Cant_casillas_a_ocupar_en_el_suelo is Len_actualizado-Tamano_max_fila,
      suelo(Jugador,Lista_puntos_negativos),
      actualizar_suelo(Lista_puntos_negativos,Cant_casillas_a_ocupar_en_el_suelo,Lista_puntos_negativos_act));
      retract(suelo(Jugador,Lista_puntos_negativos)),
      asserta(suelo(Jugador,Lista_puntos_negativos_act)));

      Fila_sgt is Fila - 1,
      pasar_Azulejo_tablero(Jugador,X,Cant_X,Fila_sgt,Fila_sgt,Penalizacion).

actualizar_suelo(L1,0,L1):-!.
actualizar_suelo(L1,Cant,Lista_actualizada):-
      add(1,L1,L1_actualizado),
      Cant_sgt is Cant-1,
      actualizar_suelo(L1_actualizado,Cant_sgt,Lista_actualizada_sgt),
      Lista_actualizada_sgt = Lista_actualizada.


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
