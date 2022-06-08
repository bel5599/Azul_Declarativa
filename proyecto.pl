:-[metodos_utiles].
:-dynamic muro/2,
          espacio_preparacion/6,
          suelo/2,
          puntuacion/2,
          total_de_Azulejos/5,
          centro/1,
          loseta/2,
          tapa/5,
          cantidad_filas_completadas/2,
          jugadores_ganadores/1,
          color_existe_en_esta_loseta/5,
          relacion_loseta_jugador_color_actual/7,
          ficha_Jugador_Inicial/1,
          total_de_losetas/1.

azulejo(1,rojo).
azulejo(2,amarillo).
azulejo(3,blanco).
azulejo(4,negro).
azulejo(5,azul).

total_de_Azulejos(20,20,20,20,20).

inicio_Simulacion_Jugadores_Losetas(2,5).
inicio_Simulacion_Jugadores_Losetas(3,7).
inicio_Simulacion_Jugadores_Losetas(4,9).

posicion_color_muro(0,0,5).
posicion_color_muro(0,1,2).
posicion_color_muro(0,2,1).
posicion_color_muro(0,3,4).
posicion_color_muro(0,4,3).

posicion_color_muro(1,0,3).
posicion_color_muro(1,1,5).
posicion_color_muro(1,2,2).
posicion_color_muro(1,3,1).
posicion_color_muro(1,4,4).

posicion_color_muro(2,0,4).
posicion_color_muro(2,1,3).
posicion_color_muro(2,2,5).
posicion_color_muro(2,3,2).
posicion_color_muro(2,4,1).

posicion_color_muro(3,0,1).
posicion_color_muro(3,1,4).
posicion_color_muro(3,2,3).
posicion_color_muro(3,3,5).
posicion_color_muro(3,4,2).

posicion_color_muro(4,0,2).
posicion_color_muro(4,1,1).
posicion_color_muro(4,2,4).
posicion_color_muro(4,3,3).
posicion_color_muro(4,4,5).

startkk:-
      inicio_Simulacion(2).
/*Metodo inicial donde se prepara la partida*/
inicio_Simulacion(Cant_Jugadores):-
    inicio_Simulacion_Jugadores_Losetas(Cant_Jugadores,Cantidad_Losetas),
        asserta(centro([])),
        asserta(tapa(0,0,0,0,0)),
        asserta(total_de_losetas(Cantidad_Losetas)),
        total_de_losetas(Cant),

        format("Total de losetas ~n"),
        write(Cant),
        format("~n"),

        Len is Cant_Jugadores+1,
        random(1,Len,Jugador_Inicial),
        asserta(ficha_Jugador_Inicial(Jugador_Inicial)),
        add(Jugador_Inicial,[],Lista_Jugadores),

        crear_lista_Jugadores(Jugador_Inicial,1,1,Cant_Jugadores,Lista_Jugadores,Jugadores),
        Losetas_centro is Cantidad_Losetas,
        crear_Lista_Losetas(0,Losetas_centro,[],Losetas),

        llenar_Losetas(Losetas),
        
        inicializar_cantidad_filas_completadas(Jugadores),
        asignar_Tablero_Jugador(Jugadores),
        imprimir_estado(Jugadores,Losetas),
        desarrollo_de_la_Partida(Jugadores,Losetas).

imprimir_estado(Jugadores,Losetas):-
        format("Jugadores ~n"),
        write(Jugadores),
        format("~n"),

        format("Losetas ~n"),
        write(Losetas),
        format("~n"),
        imprimir_losetas(Losetas),
        imprimir_tablero(Jugadores),
        format("Centro ~n"),
        centro(W),
        write(W),
        format("~n").

imprimir_losetas([]):-!.
imprimir_losetas([X]):-
        total_de_losetas(Cant),
        X==Cant,
        centro(W),
        format("Centro: "),
        write(W),
        format("~n").
imprimir_losetas([L1|Resto]):-
        loseta(L1,W),
        format("Loseta ~a:  ",[L1]),
        
        write(W),
        format("~n"),
        imprimir_losetas(Resto).
imprimir_estado_jugador(J1):-
        espacio_preparacion(J1,F1,F2,F3,F4,F5),
        Lista=[F1,F2,F3,F4,F5],
        format("JUGADOR ~a ~n",[J1]),

        format("ESPACIO DE PREPARACION ~n"),
        write(Lista),
        format("~n"),

        muro(J1,Tablero_boleano),
        format("MURO ~n"),
        write(Tablero_boleano),
        format("~n"),

        suelo(J1,Valor),
        format("SUELO ~n"),
        write(Valor),
        format("~n"),

        format("PUNTUACION ~n"),
        puntuacion(J1,Valor_punt),
        write(Valor_punt),
        format("~n").
imprimir_tablero([]):-!.
imprimir_tablero([J1|Resto]):-
        espacio_preparacion(J1,F1,F2,F3,F4,F5),
        Lista=[F1,F2,F3,F4,F5],
        format("JUGADOR ~a ~n",[J1]),

        format("ESPACIO DE PREPARACION ~n"),
        write(Lista),
        format("~n"),

        muro(J1,Tablero_boleano),
        format("MURO ~n"),
        write(Tablero_boleano),
        format("~n"),

        suelo(J1,Valor),
        format("SUELO ~n"),
        write(Valor),
        format("~n"),

        format("PUNTUACION ~n"),
        puntuacion(J1,Valor_punt),
        write(Valor_punt),
        format("~n"),

        imprimir_tablero(Resto).

crear_lista_Jugadores(_,_,Cant_Jugadores,Cant_Jugadores,Lista_en_proceso,Lista_resultante):-
        Lista_resultante=Lista_en_proceso.
crear_lista_Jugadores(Jugador_Inicial,Jugador_Inicial,Cant_elementos,Cant_Jugadores,Lista_en_proceso,Lista_resultante):-
        Pos_actual_sgt is Jugador_Inicial+1,!,
        crear_lista_Jugadores(Jugador_Inicial,Pos_actual_sgt,Cant_elementos,Cant_Jugadores,Lista_en_proceso,Lista_resultante_sgt),!,
        Lista_resultante=Lista_resultante_sgt.
        
    
crear_lista_Jugadores(Jugador_Inicial,Pos_actual,Cant_elementos,Cant_Jugadores,Lista_en_proceso,Lista_resultante):-
        add(Pos_actual,Lista_en_proceso,Lista_en_proceso_act),
        Pos_actual_sgt is Pos_actual+1,
        Cant_elementos_sgt is Cant_elementos+1,
        crear_lista_Jugadores(Jugador_Inicial,Pos_actual_sgt,Cant_elementos_sgt,Cant_Jugadores,Lista_en_proceso_act,Lista_resultante_sgt),
        Lista_resultante=Lista_resultante_sgt.
/*OK*/
crear_Lista_Losetas(Pos_Actual,Cant_Losetas,Lista_actual,Lista_Losetas):-
        Pos_Actual=:=Cant_Losetas,
        centro(W),
        length(W,Len),
        Len==1,
        nth0(0,W,-1),
        Lista_Losetas=Lista_actual.
        /*Valor==(-1).*/
        
crear_Lista_Losetas(Pos_Actual,Cant_Losetas,Lista_actual,Lista_Losetas):-
        Pos_Actual=:=Cant_Losetas,
        centro(W),
        length(W,Len),
        Len>0,
        add(Pos_Actual,Lista_actual,Lista_actual_actualizada),
        Lista_Losetas=Lista_actual_actualizada.

crear_Lista_Losetas(Pos_Actual,Cant_Losetas,Lista_actual,Lista_Losetas):-
        Pos_Actual=:=Cant_Losetas,
        Lista_Losetas=Lista_actual.
    
crear_Lista_Losetas(Pos_Actual,Cant_Losetas,Lista_actual,Lista_Losetas):-
        add(Pos_Actual,Lista_actual,Lista_actual_actualizada),
        Pos_Actual_sgt is Pos_Actual+1,
        crear_Lista_Losetas(Pos_Actual_sgt,Cant_Losetas,Lista_actual_actualizada,Lista_Losetas).

    
llenar_Losetas([]):-!.
llenar_Losetas([Loseta|Resto]):-
        asignacion_Azulejos_Loseta(W),
        asserta(loseta(Loseta,W)),
    
        write(loseta(Loseta,W)),
        format("~n"),
        llenar_Losetas(Resto),!.
    
inicializar_cantidad_filas_completadas([]):-!.
inicializar_cantidad_filas_completadas([J1|Resto]):-
        asserta(cantidad_filas_completadas(J1,0)),
        inicializar_cantidad_filas_completadas(Resto),!.
    
asignar_Tablero_Jugador([]):-!.
asignar_Tablero_Jugador([J|Resto]):-
        asserta(puntuacion(J,0)),
        asserta(muro(J,
                [[0,0,0,0,0],
                [0,0,0,0,0],
                [0,0,0,0,0],
                [0,0,0,0,0],
                [0,0,0,0,0]])),
              F1 = [],
      
              
        asserta(espacio_preparacion(J,F1,F1,F1,F1,F1)),
        asserta(suelo(J,[])),
        asignar_Tablero_Jugador(Resto).
    
        /*funcion donde se asigna azulejos a cada loseta*/
asignacion_Azulejos_Loseta(Loseta_Llena):-
        asignacion(Y1),
        asignacion(Y2),
        asignacion(Y3),
        asignacion(Y4),
        Loseta_Llena=[Y1,Y2,Y3,Y4].
    
asignacion(Y):-
        asignar_azulejo_Loseta(Y_temp,Bolsa_y_tapa_vacia1),
        Bolsa_y_tapa_vacia1=='False',
        Y=Y_temp.
      
asignacion(Y):-
        Y=0.
    
asignar_azulejo_Loseta(Azulejo,Game_over):-
        azulejos_en_la_bolsa(Bolsa_no_vacia),
        Bolsa_no_vacia=='True',
        buscar_Azulejo_en_la_bolsa(Y),
        actualizar_cantidad_Azulejos(Y),
        Azulejo=Y,
        Game_over='False'.
asignar_azulejo_Loseta(Azulejo,Game_over):-
        rellenar_azulejos_tapa_bolsa(Bolsa_no_vacia),
        Bolsa_no_vacia=='True',
        buscar_Azulejo_en_la_bolsa(Y),
        actualizar_cantidad_Azulejos(Y),
        Azulejo=Y,
        Game_over='False'.
asignar_azulejo_Loseta(_,'True').
    
/* funcion donde se pasan los azulejos de la tapa para la bolsa una vez que este vacia la bolsa*/
rellenar_azulejos_tapa_bolsa(Tapa_vacia):-
        tapa(R,Am,B,N,Az),
        Total is R+Am+B+N+Az,
        Total==0,
        Tapa_vacia='True'.
rellenar_azulejos_tapa_bolsa(Tapa_vacia):-
        tapa(R,Am,B,N,Az),
        total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,Azules),
        Rojos_Act is Rojos+R,
        Amarillos_Act is Amarillos+Am,
        Blancos_Act is Blancos+B,
        Negros_Act is Negros+N,
        Azules_Act is Azules+Az,
        retract(tapa(R,Am,B,N,Az)),
        asserta(tapa(0,0,0,0,0)),
        retract(total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,Azules)),
        asserta(total_de_Azulejos(Rojos_Act,Amarillos_Act,Blancos_Act,Negros_Act,Azules_Act)),
        Tapa_vacia='False'.
    
/*funcion donde se verfifica si la bolsa de azulejos esta vacia o no*/
azulejos_en_la_bolsa(Bolsa_no_vacia):-
        total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,Azules),
        Total is Rojos+Amarillos+Blancos+Negros+Azules,
        
        Total==0,
        Bolsa_no_vacia='False'.
azulejos_en_la_bolsa('True').
    
/*sacar un azulejo de la bolsa*/
buscar_Azulejo_en_la_bolsa(Azulejo):-
        total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,Azules),
        random(1,6,Color),
        Cant_Azulejos =[Rojos,Amarillos,Blancos,Negros,Azules],
        Pos is Color -1,
        nth0(Pos,Cant_Azulejos,Cant),
        ((Cant>0,
        Azulejo = Color);
        (buscar_Azulejo_en_la_bolsa(Azulejo))).

/*funcion que actualiza la cantidad que azulejos que hay en la bolsa cuando se saco uno*/
actualizar_cantidad_Azulejos(1):-
        total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,Azules),
        retract(total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,Azules)),
        RojosN is Rojos -1,
        asserta(total_de_Azulejos(RojosN,Amarillos,Blancos,Negros,Azules)).
    
actualizar_cantidad_Azulejos(2):-
        total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,Azules),
        retract(total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,Azules)),
        AmarillosN is Amarillos -1,
        asserta(total_de_Azulejos(Rojos,AmarillosN,Blancos,Negros,Azules)).
    
actualizar_cantidad_Azulejos(3):-
        total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,Azules),
        retract(total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,Azules)),
        BlancosN is Blancos -1,
        asserta(total_de_Azulejos(Rojos,Amarillos,BlancosN,Negros,Azules)).
    
actualizar_cantidad_Azulejos(4):-
        total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,Azules),
        retract(total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,Azules)),
        NegrosN is Negros -1,
        asserta(total_de_Azulejos(Rojos,Amarillos,Blancos,NegrosN,Azules)).
    
actualizar_cantidad_Azulejos(5):-
        total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,Azules),
        retract(total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,Azules)),
        AzulesN is Azules -1,
        asserta(total_de_Azulejos(Rojos,Amarillos,Blancos,Negros,AzulesN)).
    
        
/*SEGUNDA PARTE*/
/*funcion donde se desarrolla el juego*/
desarrollo_de_la_Partida(Jugadores,Losetas):-
        centro(W),

        add(-1,W,W_actualizado),
        retract(centro(W)),
        asserta(centro(W_actualizado)),

        desarrollar_fase_I(Jugadores,Losetas),
        
        format("Inicio de la fase II ~n"),
        fase_II(Jugadores),
        imprimir_tablero(Jugadores),

        fila_llena_final_de_la_partida(Jugadores,Losetas).
        
fila_llena_final_de_la_partida(Jugadores,_):-
        fila_Muro_Completada(Jugadores,Fila_llena),
        Fila_llena=='True',
        fin_partida(Jugadores),
        jugadores_ganadores(Jugadores,Ganadores),
        asserta(jugadores_ganadores(Ganadores)),
        format("El Jugador Ganador o los Jugadores son ~n"),
        write(Ganadores).
fila_llena_final_de_la_partida(Jugadores,Losetas):-
        fase_III(Jugadores,Losetas).

/*funcion que se encarga de ejecutar la fase I mientras hayan azulejos en las losetas o centro*/
desarrollar_fase_I(Jugadores,Losetas):-
        fase_I(Jugadores,Losetas),

        /*format("Finaliza la ronda ~n"),*/

        actualizar_jugador_inicial,
        imprimir_estado(Jugadores,Losetas),
        desarrollar_fase_I_losetas_vacias(Jugadores,Losetas),!.

desarrollar_fase_I_losetas_vacias(_,Losetas):-
        losetas_vacias(Losetas,Losetas_vacias),
        Losetas_vacias=='True',!.
        
        /*format("Losetas y centro vacios ~n"),!.*/
desarrollar_fase_I_losetas_vacias(Jugadores,_):-

        /*format("Inicializando la posicion de los Jugadores y la cantidad de Losetas a utilizar para la proxima ronda ~n"),*/
        centro(W),

        add(-1,W,W_actualizado),
        retract(centro(W)),
        asserta(centro(W_actualizado)),
        ficha_Jugador_Inicial(Jugador_Inicial),

        length(Jugadores,Cant_Jugadores),
        add(Jugador_Inicial,[],Lista_Jugadores),

        crear_lista_Jugadores(Jugador_Inicial,1,1,Cant_Jugadores,Lista_Jugadores,Jugadores_act),!,
        total_de_losetas(Cantidad_Losetas),
        Losetas_centro is Cantidad_Losetas,
        crear_Lista_Losetas(0,Losetas_centro,[],Losetas_act),!,
        desarrollar_fase_I(Jugadores_act,Losetas_act),!.

eliminar_azulejo_suelo([_|Y],[Y]).

/*funcion que verifica si hay azulejos en las losetas o centro*/
losetas_vacias([],'True'):-!.
losetas_vacias([L1|Resto],Losetas_vacias):-
        total_de_losetas(Total),
        L1==Total,
        centro_vacio(Centro_vacio),
        Centro_vacio=='True',
        losetas_vacias(Resto,Losetas_vacias),!.
losetas_vacias([L1|Resto],Losetas_vacias):-
        total_de_losetas(Total),
        L1=\=Total,
        loseta(L1,W),
        length(W,Len),
        loseta_vacia(Len,Resto,Losetas_vacias),!.
        
loseta_vacia(Len,Resto,Losetas_vacias):-
        Len==0,
        losetas_vacias(Resto,Losetas_vacias),!.
loseta_vacia(_,_,'False').



centro_vacio(Centro_vacio):-
        centro(W),
        length(W,Len),
        Len==0,
        Centro_vacio='True',!.
centro_vacio(Centro_vacio):-
        centro(W),
        length(W,Len),
        Len==1,
        nth0(0,W,-1),
        Centro_vacio='True',!.
centro_vacio('False').

/*funcion donde se ejecuta la fase I del juego*/
fase_I([],_):-!.
fase_I([Jugador_Inicial|Resto],Losetas):-
        losetas_vacias(Losetas,Losetas_vacias),
        Losetas_vacias=='False',
        /*format("Jugador ~a empieza a escoger sus azulejos para ubicarlos en la Zona de Preparacion de su Tablero ~n",[Jugador_Inicial]),
        format("Estado antes del Jugador ~n"),
        imprimir_estado_jugador(Jugador_Inicial),*/

        seleccion_Loseta(Jugador_Inicial,Losetas,Loseta_seleccionada,Loseta_optima),
        fase_I_seleccion_loseta([Jugador_Inicial|Resto],Losetas,Loseta_seleccionada,Loseta_optima),!.
fase_I(_,_).
fase_I_seleccion_loseta([Jugador_Inicial|Resto],Losetas,Loseta,Loseta_optima):-
        total_de_losetas(Cantidad_Losetas),
        Loseta<Cantidad_Losetas,

        /*format("Loseta ~a seleccionada ~n",[Loseta]),

        imprimir_estado([Jugador_Inicial|Resto],Losetas),

        format("Actualizar Tablero del Jugador Actual ~n"),*/
        actualizar_Loseta_Tablero_Centro(Jugador_Inicial,Loseta,'False',Loseta_optima),
        loseta(Loseta,W),
        retract(loseta(Loseta,W)),
        asserta(loseta(Loseta,[])),
        
        format("Estado del Jugador ~n"),
        imprimir_estado_jugador(Jugador_Inicial),

        fase_I(Resto,Losetas),!.
fase_I_seleccion_loseta([Jugador_Inicial|Resto],Losetas,Loseta,Loseta_optima):-

        /*format("El Jugador ha seleccionado el centro para elegir los azulejos a escoger ~n"),*/
        actualizar_Loseta_Tablero_Centro(Jugador_Inicial,Loseta,'True',Loseta_optima),

        /*format("Estado del Jugador ~n"),*/
        imprimir_estado_jugador(Jugador_Inicial),

        fase_I(Resto,Losetas),!.


actualizar_jugador_inicial:-
        centro(W),
        se_encuentra_ficha_jugador_inicial(W,Se_encuentra),
        Se_encuentra=='True',
        suelo(Jugador_Inicial,W1),
        actualizar_suelo(W1,1,W_actualizado),
        retract(suelo(Jugador_Inicial,W1)),
        asserta(suelo(Jugador_Inicial,W_actualizado)),
        ficha_Jugador_Inicial(J1),
        retract(ficha_Jugador_Inicial(J1)),
        asserta(ficha_Jugador_Inicial(Jugador_Inicial)),

        /*format("El Jugador inicial es ~a ~n",[Jugador_Inicial]),*/

        actualizar_centro(-1),!.
actualizar_jugador_inicial(_).

/*funcion donde se escoge la loseta a utilizar para seleccionar los azulejos*/
seleccion_Loseta(Jugador,Losetas,Loseta_seleccionada,Loseta_optima):-
        losetas_con_menor_penalizacion(Jugador,Losetas,Losetas_optimas),
        seleccion(Losetas,Losetas_optimas,Loseta_seleccionada,Loseta_optima),!.
        
seleccion(_,Losetas_optimas,Loseta_seleccionada,Loseta_optima):-
        length(Losetas_optimas,Len),
        Len>0,
        random(0,Len,Pos),
        nth0(Pos,Losetas_optimas,Loseta_seleccionada),
        Loseta_optima='True',!.
seleccion(Losetas,Losetas_optimas,Loseta_seleccionada,'False'):-
        total_de_losetas(Cant),
        Total is Cant + 1,
        random(0,Total,Pos),
        nth0(Pos,Losetas,Loseta),
        seleccion_Centro_o_Loseta(Losetas,Losetas_optimas,Loseta,Loseta_seleccionada,'False'),!.
        
seleccion_Centro_o_Loseta(_,_,Loseta,Loseta_seleccionada,'False'):-
        total_de_losetas(Total),
        Loseta=\=Total,
        loseta(Loseta,W),
        length(W,Len),
        Len>0,
        Loseta_seleccionada=Loseta,!.
seleccion_Centro_o_Loseta(_,_,Loseta,Loseta_seleccionada,'False'):-
        total_de_losetas(Total),
        Loseta==Total,
        centro(W),
        length(W,Len),
        Len>0,
        Loseta_seleccionada=Loseta,!.
seleccion_Centro_o_Loseta(Losetas,Losetas_optimas,_,Loseta_seleccionada,'False'):-
        seleccion(Losetas,Losetas_optimas,Loseta_seleccionada,'False'),!.

/*funcion que devuelve las losetas que completan una fila con la menor penalizacion*/
losetas_con_menor_penalizacion(Jugador,[L1|Resto],Losetas_optimas):-
        total_de_losetas(Cant),
        L1==Cant,
        centro(W),
        length(W,Len),
        Len>0,
        losetas_que_completan_filas(Jugador,[L1|Resto],[],Losetas_que_completan_filas),

        extraer_columna(Losetas_que_completan_filas,1,Penalizacion_por_Loseta),
        penalizacion_minima(Penalizacion_por_Loseta,Penalizacion_minima),
        losetas_optimas(Losetas_que_completan_filas,Penalizacion_minima,[],Losetas_optimas),!.
losetas_con_menor_penalizacion(Jugador,[L1|Resto],Losetas_optimas):-
        loseta(L1,W),
        length(W,Len),
        Len>0,
        losetas_que_completan_filas(Jugador,[L1|Resto],[],Losetas_que_completan_filas),

        extraer_columna(Losetas_que_completan_filas,1,Penalizacion_por_Loseta),
        penalizacion_minima(Penalizacion_por_Loseta,Penalizacion_minima),
        losetas_optimas(Losetas_que_completan_filas,Penalizacion_minima,[],Losetas_optimas),!.
losetas_con_menor_penalizacion(Jugador,[_|Resto],Losetas_optimas):-
        losetas_con_menor_penalizacion(Jugador,Resto,Losetas_optimas),!.

/*funcion en la que se haya la loseta con la menor penalizacion*/
penalizacion_minima([],-100):-!.
penalizacion_minima([L1|Resto],Penalizacion_minima):-
        penalizacion_minima(Resto,Penalizacion_minima_sgt),
        Penalizacion_minima_sgt>L1,
        Penalizacion_minima = Penalizacion_minima_sgt,!.
penalizacion_minima([L1|_],Penalizacion_minima):-
        Penalizacion_minima=L1,!.
/*funcion que devuelve todas las losetas que completan una fila  y tienen menor penalizacion */
losetas_optimas([],_,Losetas_optimas_en_proceso,Losetas_optimas_en_proceso):-!.
losetas_optimas([L1|Resto],Penalizacion_minima,Losetas_optimas_en_proceso,Losetas_optimas):-

        nth0(1,L1,Penalizacion_loseta_actual),
        nth0(0,L1,Loseta_actual),
        Penalizacion_loseta_actual==Penalizacion_minima,
        add(Loseta_actual,Losetas_optimas_en_proceso,Losetas_optimas_en_proceso_actualizada),

        losetas_optimas(Resto,Penalizacion_minima,Losetas_optimas_en_proceso_actualizada,Losetas_optimas_actualizada),
        Losetas_optimas=Losetas_optimas_actualizada,!.
     
losetas_optimas([_|Resto],Penalizacion_minima,Losetas_optimas_en_proceso,Losetas_optimas):-
        losetas_optimas(Resto,Penalizacion_minima,Losetas_optimas_en_proceso,Losetas_optimas_actualizada),
        Losetas_optimas=Losetas_optimas_actualizada,!.

/*funcion donde se devuelven las loseta que completan una fila*/
losetas_que_completan_filas(_,[],Losetas_en_proceso,Losetas_en_proceso):-!.
losetas_que_completan_filas(Jugador,[L1|Resto],Losetas_en_proceso,Losetas_optimas):-
        cant_colores_loseta(L1,Cant_apariciones_de_color),
        completar_fila_zona_preparacion_loseta(L1,Cant_apariciones_de_color,Jugador,Completa_fila,Penalizacion),
        losetas_que_completan_filas(Jugador,[L1|Resto],Completa_fila,Penalizacion,Losetas_en_proceso,Losetas_optimas),!.

losetas_que_completan_filas(Jugador,[L1|Resto],Completa_fila,Penalizacion,Losetas_en_proceso,Losetas_optimas):-
        Completa_fila=='True',
        add([L1,Penalizacion],Losetas_en_proceso,Losetas_en_proceso_actualizada),
        losetas_que_completan_filas(Jugador,Resto,Losetas_en_proceso_actualizada,Losetas_optimas_actualizada),
        Losetas_optimas=Losetas_optimas_actualizada,!.
losetas_que_completan_filas(Jugador,[_|Resto],_,_,Losetas_en_proceso,Losetas_optimas):-
        losetas_que_completan_filas(Jugador,Resto,Losetas_en_proceso,Losetas_optimas_actualizada),
        Losetas_optimas=Losetas_optimas_actualizada,!.

/*funcion que devuelve la cantidad de veces que esta un azulejo de color especifico en una loseta*/
cant_colores_loseta(L1,Cant_apariciones_de_color):-
        total_de_losetas(Total),
        Total=:=L1,
        centro(W),
        eliminar_azulejo_centro(W,-1,[],W_actualizado),
        length(W_actualizado,Len),
        Tamano = Len,
        cant_colores_loseta_recursivo(L1,W_actualizado,0,Tamano),
        cant_apariciones(L1,W_actualizado,0,[],Cant_apariciones_de_color),
        actualizar_base_datos,!.

cant_colores_loseta(L1,Cant_apariciones_de_color):-
        loseta(L1,W),
        length(W,Len),
        Tamano=Len,
        cant_colores_loseta_recursivo(L1,W,0,Tamano),
        cant_apariciones(L1,W,0,[],Cant_apariciones_de_color),
        actualizar_base_datos,!.

/*funcion recursiva que busca la cantidad de apariciones de un azulejo de color especifico*/
cant_apariciones(_,W,Pos_actual,Lista_en_proceso,Lista_resultante):-
        length(W,Len),
        Pos_actual==Len,
        Lista_resultante=Lista_en_proceso,!.
cant_apariciones(L1,W,Pos_actual,Lista_en_proceso,Lista_resultante):-
        nth0(Pos_actual,W,Item),
        color_existe_en_esta_loseta(L1,Pos_actual,Item,_,Cant_apariciones),
        add(Cant_apariciones,Lista_en_proceso,Lista_en_proceso_act),
        Pos_actual_sgt is Pos_actual+1,
        cant_apariciones(L1,W,Pos_actual_sgt,Lista_en_proceso_act,Lista_resultante_sgt),
        Lista_resultante=Lista_resultante_sgt,!.

/*funcion recursiva que busca la cantidad de colores de azulejos por losetas*/
cant_colores_loseta_recursivo(_,_,Cant_Azulejos,Cant_Azulejos):-!.

cant_colores_loseta_recursivo(L1,W,Pos_actual,Cant_azulejos_loseta_Actual):-
        nth0(Pos_actual,W,Color),
        color_actual_igual_a_otro_ya_existente(0,Pos_actual,Color,L1,W),
        color_existe_en_esta_loseta(L1,Pos_actual,Color,_,_),
        Pos_Actual_sgt is Pos_actual+1,
        cant_colores_loseta_recursivo(L1,W,Pos_Actual_sgt,Cant_azulejos_loseta_Actual),!.

/*funcion que actualiza la cantidad de colores si hay mas de 1 azulejo del mismo color */
color_actual_igual_a_otro_ya_existente(0,0,_,_,[0|_]):-!.
color_actual_igual_a_otro_ya_existente(0,0,_,L1,[Item|_]):-
        asserta(color_existe_en_esta_loseta(L1,0,Item,'True',1)),!.
  
color_actual_igual_a_otro_ya_existente(Pos_color_actual,Pos_color_actual,_,L1,W):-

        nth0(Pos_color_actual,W,Item),
        asserta(color_existe_en_esta_loseta(L1,Pos_color_actual,Item,'True',1)),!.
  
color_actual_igual_a_otro_ya_existente(Pos_inicial,Pos_color_actual,Color,L1,W):-

        nth0(Pos_inicial,W,Item),
        Item==Color,
        color_existe_en_esta_loseta(L1,Pos_inicial,Item,'True',X1),
        retract(color_existe_en_esta_loseta(L1,Pos_inicial,Item,'True',X1)),
        X2 is X1+1,
        asserta(color_existe_en_esta_loseta(L1,Pos_inicial,Item,'True',X2)),

        asserta(color_existe_en_esta_loseta(L1,Pos_color_actual,Color,'False',0)),!.

color_actual_igual_a_otro_ya_existente(Pos_inicial,Pos_color_actual,Color,L1,W):-
        Pos_inicial_sgt is Pos_inicial+1,
        color_actual_igual_a_otro_ya_existente(Pos_inicial_sgt,Pos_color_actual,Color,L1,W),!.

/*borra toda cláusula de la base de datos cuya cabeza unifique con Head*/
actualizar_base_datos():-
        retractall(color_existe_en_esta_loseta),!.

/*verifica si la loseta actual llena un fila*/
completar_fila_zona_preparacion_loseta(L1,Cant_apariciones_de_color,J1,Completa_fila,Penalizacion):-
        total_de_losetas(Total),
        L1==Total,
        centro(W),
        eliminar_azulejo_centro(W,-1,[],W_actualizado),
        completar_fila_zona_preparacion_loseta_recursivo(L1,Cant_apariciones_de_color,W_actualizado,0,J1,Completa_fila,0,Penalizacion),!.
completar_fila_zona_preparacion_loseta(L1,Cant_apariciones_de_color,J1,Completa_fila,Penalizacion):-
        loseta(L1,Colores),
        completar_fila_zona_preparacion_loseta_recursivo(L1,Cant_apariciones_de_color,Colores,0,J1,Completa_fila,0,Penalizacion),!.

/*funcion recursiva que verifica si la loseta actual llena un fila*/
completar_fila_zona_preparacion_loseta_recursivo(_,_,Azulejos,Pos_actual,_,Completa_fila,Penalizacion_actual,Penalizacion):-
        length(Azulejos,Len),
        Pos_actual==Len,
        Completa_fila='False',
        Penalizacion=Penalizacion_actual,!.
completar_fila_zona_preparacion_loseta_recursivo(L1,Cant_apariciones_de_color,Azulejos_loseta,Posicion_actual,J1,Completa_fila,Penalizacion_en_proceso, Penalizacion):-
        nth0(Posicion_actual,Azulejos_loseta,Item),
        nth0(Posicion_actual,Cant_apariciones_de_color,Cant_Item),
        completar_fila_zona_preparacion_loseta_cada_color(Item,Cant_Item,J1,Completa_fila_Item,Penalizacion_Item),!,
        asserta(relacion_loseta_jugador_color_actual(J1,L1,Posicion_actual,Item,Cant_Item,Completa_fila_Item,Penalizacion_Item)),

        Posicion_actual_sgt is Posicion_actual+1,
        Penalizacion_en_proceso_sgt is Penalizacion_en_proceso+Penalizacion_Item,
        completar_fila_zona_preparacion_loseta_recursivo(L1,Cant_apariciones_de_color,Azulejos_loseta,Posicion_actual_sgt,J1,Completa_fila_sgt,Penalizacion_en_proceso_sgt,Penalizacion_actualizada),
        ((Completa_fila_Item=='True',
        Completa_fila='True');
        (Completa_fila= Completa_fila_sgt)),
        Penalizacion=Penalizacion_actualizada,!.

/*funcion que verifica si el color actual llena un fila*/
completar_fila_zona_preparacion_loseta_cada_color(_,0,_,'False',0):-!.
completar_fila_zona_preparacion_loseta_cada_color(Color,Cant_apariciones,J1,Completa_fila,Penalizacion):-
        espacio_preparacion(J1,F1,F2,F3,F4,F5),
        cant_apariciones_fila_con_penalizacion(J1,Color,Cant_apariciones,[F5,F4,F3,F2,F1],5,Completa_fila,Penalizacion),!.

/*funcion que verifica si el color actual con la cantidad de apariciones se puede agregar a la fila actual*/
cant_apariciones_fila_con_penalizacion(J1,Color,Cant_apariciones,Lista_con_F1,1,Completa_fila,Penalizacion):-
        nth0(0,Lista_con_F1,F1),
        azulejo_valido_en_la_fila_actual(J1,Color,F1,1,Azulejo_valido),
        Azulejo_valido=='True',
        Completa_fila='True',
        apariciones_con_penalizacion(J1,Cant_apariciones,1,Penalizacion).

cant_apariciones_fila_con_penalizacion(_,_,_,_,1,'False',0):-!.

cant_apariciones_fila_con_penalizacion(J1,Color,Cant_apariciones,[Fila_Actual|Resto],Tamano_max_fila_Actual,Completa_fila,Penalizacion):-
        azulejo_valido_en_la_fila_actual(J1,Color,Fila_Actual,Tamano_max_fila_Actual,Azulejo_valido),
        Azulejo_valido=='True',
        analizar_por_cada_fila(J1,Color,Cant_apariciones,[Fila_Actual|Resto],Tamano_max_fila_Actual,Completa_fila,Penalizacion),!.

cant_apariciones_fila_con_penalizacion(J1,Color,Cant_apariciones,[_|Resto],Tamano_max_fila_Actual,Completa_fila,Penalizacion):-
        Tamano_max_fila_Actual_sgt is Tamano_max_fila_Actual - 1,
        cant_apariciones_fila_con_penalizacion(J1,Color,Cant_apariciones,Resto,Tamano_max_fila_Actual_sgt,Completa_fila_sgt,Penalizacion_sgt),
        Completa_fila=Completa_fila_sgt,
        Penalizacion=Penalizacion_sgt,!.

analizar_por_cada_fila(J1,_,Cant_apariciones,[Fila_Actual|_],Tamano_max_fila_Actual,Completa_fila,Penalizacion):-
        length(Fila_Actual,Len),
        Len_actualizado is Len + Cant_apariciones,
        Len_actualizado>=Tamano_max_fila_Actual,
        Completa_fila='True',
        apariciones_con_penalizacion(J1,Len_actualizado,Tamano_max_fila_Actual,Penalizacion),!.
analizar_por_cada_fila(J1,Color,Cant_apariciones,[_|Resto],Tamano_max_fila_Actual,Completa_fila,Penalizacion):-
        Tamano_max_fila_Actual_sgt is Tamano_max_fila_Actual - 1,
        cant_apariciones_fila_con_penalizacion(J1,Color,Cant_apariciones,Resto,Tamano_max_fila_Actual_sgt,Completa_fila_sgt,Penalizacion_sgt),
        Completa_fila=Completa_fila_sgt,
        Penalizacion=Penalizacion_sgt,!.

apariciones_con_penalizacion(J1,Len_actualizado,Tamano_max_fila_Actual,Penalizacion):-
        Len_actualizado>Tamano_max_fila_Actual,

        Cant_casillas_a_ocupar_en_el_suelo is Len_actualizado-Tamano_max_fila_Actual,
        cant_puntos_negativos_en_la_posicion_actual_del_suelo(J1,Cant_casillas_a_ocupar_en_el_suelo,Puntos_negativos),
        Penalizacion= Puntos_negativos,!.
apariciones_con_penalizacion(_,_,_,0).

puntos_negativos_suelo_posicion(0,-1).
puntos_negativos_suelo_posicion(1,-1).
puntos_negativos_suelo_posicion(2,-2).
puntos_negativos_suelo_posicion(3,-2).
puntos_negativos_suelo_posicion(4,-2).
puntos_negativos_suelo_posicion(5,-3).
puntos_negativos_suelo_posicion(6,-3).

cant_puntos_negativos_en_la_posicion_actual_del_suelo(J1,Casillas_a_ocupar,Valor):-
        suelo(J1,W),
        length(W,Len),
        Pos_final is Len+Casillas_a_ocupar-1,
        total_puntos_negativos(Len,Pos_final,Valor),!.
total_puntos_negativos(X,_,0):-
        X>=7,!.
total_puntos_negativos(X,X,Valor):-
        puntos_negativos_suelo_posicion(X,Valor),!.

total_puntos_negativos(Pos_inicial,Pos_final,Valor):-
        Pos_inicial_sgt is Pos_inicial+1,
        total_puntos_negativos(Pos_inicial_sgt,Pos_final,Valor_resultante),
        puntos_negativos_suelo_posicion(Pos_inicial,Valor_actual),
        Valor is Valor_resultante + Valor_actual,!.

/*funcion que verifica si el zulejo se puede poner en la fila actual*/
azulejo_valido_en_la_fila_actual(J1,Color,Fila,Tamano_max_fila,Azulejo_valido):-
        casilla_vacias_para_ubicar_azulejos_nuevos(Fila,Tamano_max_fila,Azulejo_valido_casillas_vacias),
        Azulejo_valido_casillas_vacias=='True',
        fila_con_los_mismos_colores(Color,Fila,Azulejo_valido_fila),
        Azulejo_valido_fila=='True',

        muro(J1,Tablero),
        Pos_fila is Tamano_max_fila-1,
        nth0(Pos_fila,Tablero,Fila_muro),
        fila_muro_con_distintos_colores(Color,Fila_muro,Azulejo_valido_muro),
        Azulejo_valido_muro=='True',

        Azulejo_valido='True',!.
azulejo_valido_en_la_fila_actual(_,_,_,_,'False').
/*funcion que comprueba si hay casillas vacia en la fila actual*/
casilla_vacias_para_ubicar_azulejos_nuevos(Fila,Tamano_max_fila,Casillas_vacias_suficientes):-
        length(Fila,Len),
        Len==Tamano_max_fila,
        Casillas_vacias_suficientes='False',!.
casilla_vacias_para_ubicar_azulejos_nuevos(_,_,'True').
  
  /*funcion que verifica si en la fila actual los azulejos existentes son del mismo color que el que se quiere agregar*/
fila_con_los_mismos_colores(_,[],'True'):-!.
fila_con_los_mismos_colores(Color,[A1|Resto],Mismo_color):-
        Color==A1,
        fila_con_los_mismos_colores(Color,Resto,Mismo_color_sgt),
        Mismo_color = Mismo_color_sgt,!.
fila_con_los_mismos_colores(_,_,'False').
/*funcion que verifica si en la fila del muro correspondiente no hay azulejos del mismo color que el que se quiere agregar*/
fila_muro_con_distintos_colores(_,[],'True'):-!.
fila_muro_con_distintos_colores(Color,[A1|Resto],Diferente_color):-
        A1==0,
        fila_muro_con_distintos_colores(Color,Resto,Diferente_color),!.
fila_muro_con_distintos_colores(Color,[A1|Resto],Diferente_color):-
        Color=\=A1,
        fila_muro_con_distintos_colores(Color,Resto,Diferente_color),!.
fila_muro_con_distintos_colores(_,_,'False').

/*funcion que actualiza la cantidad de azulejos que hay en el centro*/
actualizar_Loseta_Tablero_Centro(Jugador,Loseta,'True',Loseta_optima):-
        centro(W),
        eliminar_azulejo_centro(W,-1,[],[Azulejo|Resto]),
        seleccion_Azulejos(Jugador,'True',Loseta,0,[Azulejo|Resto],Loseta_optima),
        retractall(relacion_loseta_jugador_color_actual),!.

actualizar_Loseta_Tablero_Centro(Jugador,Loseta,'False',Loseta_optima):-
        loseta(Loseta,[Azulejo|Resto]),

        seleccion_Azulejos(Jugador,'False',Loseta,0,[Azulejo|Resto],Loseta_optima),
        retract(loseta(Loseta,[Azulejo|Resto])),
        asserta(loseta(Loseta,[])),
        retractall(relacion_loseta_jugador_color_actual),!.

/*funcion que actualiz la zona de preparacion*/
seleccion_Azulejos(Jugador,Es_el_centro,Loseta_actual,Pos_actual,[X|Resto],Loseta_optima):-
        relacion_loseta_jugador_color_actual(Jugador,Loseta_actual,Pos_actual,X,Cant_X,_,_),
        Cant_X==0,
        Pos_actual_sgt is Pos_actual+1,
        seleccion_Azulejos(Jugador,Es_el_centro,Pos_actual_sgt, Loseta_actual, Resto,Loseta_optima),!.

seleccion_Azulejos(Jugador,'True',Loseta_actual,Pos_actual,[X|_],'True'):-
        relacion_loseta_jugador_color_actual(Jugador,Loseta_actual,Pos_actual,X,Cant_X,Completa_fila_X,Penalizacion_X),
        Completa_fila_X=='True',

        actualizar_centro(X),
        relacion_loseta_jugador_color_actual(Jugador,Loseta_actual,Pos_actual,X,Cant_X,_,Penalizacion_X),
        pasar_Azulejo_tablero(Jugador,X,Cant_X,4,5,'True',_),!.

seleccion_Azulejos(Jugador,'True',Loseta_actual,Pos_actual,[X|_],'False'):-
        relacion_loseta_jugador_color_actual(Jugador,Loseta_actual,Pos_actual,X,Cant_X,_,_),
        actualizar_centro(X),
        pasar_Azulejo_tablero(Jugador,X,Cant_X,4,5,'False',_),!.

seleccion_Azulejos(Jugador,'False',Loseta_actual,Pos_actual,[X|Resto],'True'):-
        relacion_loseta_jugador_color_actual(Jugador,Loseta_actual,Pos_actual,X,Cant_X,Completa_fila,_),
        Completa_fila=='True',

        pasar_Azulejo_tablero(Jugador,X,Cant_X,4,5,'True',_),
        Pos_actual_sgt is Pos_actual+1,
        annadir_azulejos_centro(Resto,Jugador,Pos_actual_sgt,Loseta_actual),!.

seleccion_Azulejos(Jugador,'True',Loseta_actual,Pos_actual,[_|Resto],'True'):-
        Pos_actual_sgt is Pos_actual+1,
        seleccion_Azulejos(Jugador,'True',Loseta_actual,Pos_actual_sgt,Resto,'True'),!.

seleccion_Azulejos(Jugador,Es_el_centro,Loseta_actual,Pos_actual,[X|Resto],'True'):-
        annadir_azulejos_centro(X,Jugador,Pos_actual,Loseta_actual),
        Pos_actual_sgt is Pos_actual+1,
        seleccion_Azulejos(Jugador,Es_el_centro,Loseta_actual,Pos_actual_sgt,Resto,'True'),!.
seleccion_Azulejos(Jugador,_,Loseta_actual,Pos_actual,[X|Resto],'False'):-
        relacion_loseta_jugador_color_actual(Jugador,Loseta_actual,Pos_actual,X,Cant_X,_,_),
        pasar_Azulejo_tablero(Jugador,X,Cant_X,4,5,'False',_),
        Pos_actual_sgt is Pos_actual+1,
        annadir_azulejos_centro(Resto,Jugador,Pos_actual_sgt,Loseta_actual),!.


pasar_Azulejo_tablero(_,_,_,-1,_,_,'False'):-!.
pasar_Azulejo_tablero(Jugador,X,Cant_X,Fila_actual,Tamanno_max_fila,Loseta_optima,Zona_preparacion_actualizada):-
        espacio_preparacion(Jugador,F1,F2,F3,F4,F5),
        Lista=[F1,F2,F3,F4,F5],
        nth0(Fila_actual,Lista,Fila),
        azulejo_valido_en_la_fila_actual(Jugador,X,Fila,Tamanno_max_fila,Azulejo_valido),

        Azulejo_valido=='True',

        pasar_Azulejo_tablero_penalizacion(X,Fila_actual,Jugador,Cant_X,Fila,Loseta_optima,Tamanno_max_fila,Zona_preparacion_actualizada),!.
  
pasar_Azulejo_tablero(Jugador,X,Cant_X,Fila_actual,Tamanno_max_fila,Loseta_optima,Zona_preparacion_actualizada):-
        Fila_actual_sgt is Fila_actual-1,
        Tamanno_max_fila_sgt is Tamanno_max_fila-1,
        pasar_Azulejo_tablero(Jugador,X,Cant_X,Fila_actual_sgt,Tamanno_max_fila_sgt,Loseta_optima,Zona_preparacion_actualizada),!.

pasar_Azulejo_tablero_penalizacion(X,Fila_actual,Jugador,Cant_X,Fila,'True',Tamanno_max_fila,Zona_preparacion_actualizada):-
        length(Fila,Len),
        Len_actualizado is Len + Cant_X,
        Len_actualizado > Tamanno_max_fila,
        Azulejos_a_pasar is Tamanno_max_fila-Len,

        espacio_preparacion(Jugador,F1,F2,F3,F4,F5),
        retract(espacio_preparacion(Jugador,F1,F2,F3,F4,F5)),
        actualizar_espacio_preparacion(Jugador,X,Azulejos_a_pasar,Fila_actual,F1,F2,F3,F4,F5),

        Cant_casillas_a_ocupar_en_el_suelo is Len_actualizado-Tamanno_max_fila,
        suelo(Jugador,Lista_puntos_negativos),
        actualizar_suelo(Lista_puntos_negativos,Cant_casillas_a_ocupar_en_el_suelo,Lista_puntos_negativos_act),
        retract(suelo(Jugador,Lista_puntos_negativos)),
        asserta(suelo(Jugador,Lista_puntos_negativos_act)),
        Zona_preparacion_actualizada='True',!.
pasar_Azulejo_tablero_penalizacion(X,Fila_actual,Jugador,Cant_X,Fila,'True',Tamanno_max_fila,Zona_preparacion_actualizada):-
        length(Fila,Len),
        Len_actualizado is Len + Cant_X,
        Len_actualizado == Tamanno_max_fila,
        espacio_preparacion(Jugador,F1,F2,F3,F4,F5),
        retract(espacio_preparacion(Jugador,F1,F2,F3,F4,F5)),
        actualizar_espacio_preparacion(Jugador,X,Cant_X,Fila_actual,F1,F2,F3,F4,F5),
        Zona_preparacion_actualizada='True',!.
pasar_Azulejo_tablero_penalizacion(X,Fila_actual,Jugador,Cant_X,_,'True',Tamanno_max_fila,Zona_preparacion_actualizada):-
        Fila_actual_sgt is Fila_actual-1,
        Tamanno_max_fila_sgt is Tamanno_max_fila-1,
        pasar_Azulejo_tablero(Jugador,X,Cant_X,Fila_actual_sgt,Tamanno_max_fila_sgt,'True',Zona_preparacion_actualizada),!.

pasar_Azulejo_tablero_penalizacion(X,Fila_actual,Jugador,Cant_X,_,'False',_,Zona_preparacion_actualizada):-
        espacio_preparacion(Jugador,F1,F2,F3,F4,F5),
        retract(espacio_preparacion(Jugador,F1,F2,F3,F4,F5)),
        actualizar_espacio_preparacion(Jugador,X,Cant_X,Fila_actual,F1,F2,F3,F4,F5),
        Zona_preparacion_actualizada='True',!.

/*OK*/
actualizar_espacio_preparacion(Jugador,X,Cant_X,0,F1,F2,F3,F4,F5):-
        annadir_azulejos_fila_preparacion(X,Cant_X,F1,F1_actualizado),
        asserta(espacio_preparacion(Jugador,F1_actualizado,F2,F3,F4,F5)),!.

actualizar_espacio_preparacion(Jugador,X,Cant_X,1,F1,F2,F3,F4,F5):-
        annadir_azulejos_fila_preparacion(X,Cant_X,F2,F2_actualizado),
        asserta(espacio_preparacion(Jugador,F1,F2_actualizado,F3,F4,F5)),!.

actualizar_espacio_preparacion(Jugador,X,Cant_X,2,F1,F2,F3,F4,F5):-
        annadir_azulejos_fila_preparacion(X,Cant_X,F3,F3_actualizado),
        asserta(espacio_preparacion(Jugador,F1,F2,F3_actualizado,F4,F5)),!.

actualizar_espacio_preparacion(Jugador,X,Cant_X,3,F1,F2,F3,F4,F5):-
        annadir_azulejos_fila_preparacion(X,Cant_X,F4,F4_actualizado),
        asserta(espacio_preparacion(Jugador,F1,F2,F3,F4_actualizado,F5)),!.

actualizar_espacio_preparacion(Jugador,X,Cant_X,4,F1,F2,F3,F4,F5):-
        annadir_azulejos_fila_preparacion(X,Cant_X,F5,F5_actualizado),
        asserta(espacio_preparacion(Jugador,F1,F2,F3,F4,F5_actualizado)),!.


/*funcion que annade azulejos a la fila actual*/
annadir_azulejos_fila_preparacion(_,0,Fila,Fila):-!.
annadir_azulejos_fila_preparacion(X,Cant_X,Fila,Fila_resultante):-
        add(X,Fila,Fila_actualizada),
        Cant_X_sgt is Cant_X -1,
        annadir_azulejos_fila_preparacion(X,Cant_X_sgt,Fila_actualizada,Fila_resultante),!.


/*funcion que annade azulejos al centro*/
annadir_azulejos_centro([],_,_,_):-!.
annadir_azulejos_centro([A1|Resto],Jugador,Pos_actual,Loseta_actual):-
        relacion_loseta_jugador_color_actual(Jugador,Loseta_actual,Pos_actual,A1,Cant_A1,_,_),
        Cant_A1==0,
        Pos_actual_sgt is Pos_actual+1,
        annadir_azulejos_centro(Resto,Jugador,Pos_actual_sgt,Loseta_actual),!.
        
annadir_azulejos_centro([A1|Resto],Jugador,Pos_actual,Loseta_actual):-
        relacion_loseta_jugador_color_actual(Jugador,Loseta_actual,Pos_actual,A1,Cant_A1,_,_),
        annadir_azulejos_centro_varias_cantidades(A1,Cant_A1),
        Pos_actual_sgt is Pos_actual+1,
        annadir_azulejos_centro(Resto,Jugador,Pos_actual_sgt,Loseta_actual),!.

push(X,Y,[X|Y]).

/*funcion que annade varios azulejos al centro*/
annadir_azulejos_centro_varias_cantidades(_,0):-!.
annadir_azulejos_centro_varias_cantidades(Azulejo,Cant):-
        centro(Azulejos_Centro),
        retract(centro(Azulejos_Centro)),
        add(Azulejo,Azulejos_Centro,Azulejos_Centro_Actualizado),
        asserta(centro(Azulejos_Centro_Actualizado)),
        Cant_sgt is Cant-1,
        annadir_azulejos_centro_varias_cantidades(Azulejo,Cant_sgt),!.

  
actualizar_suelo(L1,0,L1):-!.
actualizar_suelo(L1,Cant,Lista_actualizada):-
      add(1,L1,L1_actualizado),
      Cant_sgt is Cant-1,
      actualizar_suelo(L1_actualizado,Cant_sgt,Lista_actualizada),!.

actualizar_centro(Color):-
        centro(W),
        eliminar_azulejo_centro(W,Color,[],W_actualizado),
        retract(centro(W)),
        asserta(centro(W_actualizado)),!.

eliminar_azulejo_centro([],_,Lista_en_proceso,Lista_en_proceso):-!.
eliminar_azulejo_centro([Azulejo|Resto],Azulejo,Lista_en_proceso,Lista_resultante):-
        eliminar_azulejo_centro(Resto,Azulejo,Lista_en_proceso,Lista_resultante).
      
eliminar_azulejo_centro([Azulejo|Resto],Color,Lista_en_proceso,Lista_resultante):-
        add(Azulejo,Lista_en_proceso,Lista_en_proceso_act),
        eliminar_azulejo_centro(Resto,Color,Lista_en_proceso_act,Lista_resultante).

/*funcion booleana que verifica si se encuentra la ficha inicial en el centro*/
se_encuentra_ficha_jugador_inicial([],'False'):-!.
se_encuentra_ficha_jugador_inicial([-1|_],'True'):-!.
se_encuentra_ficha_jugador_inicial([_|Resto],Se_encuentra):-
        se_encuentra_ficha_jugador_inicial(Resto,Se_encuentra_sgt),
        Se_encuentra=Se_encuentra_sgt.

/*Fase II*/
/*asignar_color(X,Y):-azulejo(X,Y).*/

/*funcion donde empiezaa la fase II*/
fase_II([]):-!.
fase_II([J1|Resto]):-
        fase_II_fila_completa(J1,4),

        puntos_en_el_suelo(J1,Puntos_a_restar),
        puntuacion(J1,Punt),
        Puntuacion_actualizada is Punt + Puntos_a_restar,
        retract(puntuacion(J1,Punt)),
        asserta(puntuacion(J1,Puntuacion_actualizada)),

        fase_II(Resto).
fase_II_fila_completa(_,Fila_Actual):-
        Fila_Actual<0.
fase_II_fila_completa(J1,0):-
        espacio_preparacion(J1,F1,_,_,_,_),
        length(F1,Len),
        Len>0,
        nth0(0,F1,Color),
        fila_completa_espacio_preparacion(F1,0,Fila_completa),
        Fila_completa=='True',
 
        ubicar_Azulejo_Muro(Color,J1,0,Pos),
        ubicar_resto_azulejos_en_la_tapa(Color,J1,0,0),
        actualizar_puntuacion_muro(J1,0,Pos).

fase_II_fila_completa(J1,Fila_actual):-
       
        espacio_preparacion(J1,F1,F2,F3,F4,F5),
        Lista=[F1,F2,F3,F4,F5],
        nth0(Fila_actual,Lista,Fila),
        length(Fila,Len),
        Len>0,
        nth0(0,Fila,Color),
        fila_completa_espacio_preparacion(Fila,Fila_actual,Fila_completa),
        Fila_completa=='True',
 
        ubicar_Azulejo_Muro(Color,J1,Fila_actual,Pos),
        ubicar_resto_azulejos_en_la_tapa(Color,J1,Fila_actual,Fila_actual),
        actualizar_puntuacion_muro(J1,Fila_actual,Pos),
        Fila_actual_sgt is Fila_actual-1,
        fase_II_fila_completa(J1,Fila_actual_sgt).
 
fase_II_fila_completa(J1,Fila_actual):-
        Fila_actual_sgt is Fila_actual-1,
        fase_II_fila_completa(J1,Fila_actual_sgt),!.

puntos_en_el_suelo(Jugador,Puntuacion_a_restar):-
        suelo(Jugador,W),
        puntos_actual_en_el_suelo(W,0,Puntuacion_a_restar),
        retract(suelo(Jugador,W)),
        asserta(suelo(Jugador,[])).

puntos_actual_en_el_suelo([],_,0):-!.
puntos_actual_en_el_suelo([_|_],Pos_Actual,0):-
        Pos_Actual>=7,!.
puntos_actual_en_el_suelo([_|Resto],Pos_actual,Puntuacion):-
        puntos_negativos_suelo_posicion(Pos_actual,Valor),
        Pos_actual_sgt is Pos_actual +1,
        puntos_actual_en_el_suelo(Resto,Pos_actual_sgt,Puntuacion_sgt),
        Puntuacion is Puntuacion_sgt + Valor.


/*funcion que ubica los restantes azulejos que sobran de la fila de preparacion actual a la tapa*/
ubicar_resto_azulejos_en_la_tapa(_,Jugador,0,0):-
        espacio_preparacion(Jugador,F1,F2,F3,F4,F5),
        retract(espacio_preparacion(Jugador,F1,F2,F3,F4,F5)),
        vaciar_fila_zona_preparacion(0,Jugador,F1,F2,F3,F4,F5).
        
ubicar_resto_azulejos_en_la_tapa(Color,Jugador,Fila_espacio_preparacion,Cant_azulejos):-
        tapa(Rojos,Amarillos,Blancos,Negros,Azules),
        retract(tapa(Rojos,Amarillos,Blancos,Negros,Azules)),
        ubicar_resto_azulejos(Color,Cant_azulejos,Rojos,Amarillos,Blancos,Negros,Azules),
        espacio_preparacion(Jugador,F1,F2,F3,F4,F5),
        retract(espacio_preparacion(Jugador,F1,F2,F3,F4,F5)),
        vaciar_fila_zona_preparacion(Fila_espacio_preparacion,Jugador,F1,F2,F3,F4,F5).
        
ubicar_resto_azulejos(1,Cant_azulejos,Rojos,Amarillos,Blancos,Negros,Azules):-
        Rojos_Actualizado is Rojos+Cant_azulejos,
        asserta(tapa(Rojos_Actualizado,Amarillos,Blancos,Negros,Azules)),!.

ubicar_resto_azulejos(2,Cant_azulejos,Rojos,Amarillos,Blancos,Negros,Azules):-
        Amarillos_Actualizado is Amarillos+Cant_azulejos,
        asserta(tapa(Rojos,Amarillos_Actualizado,Blancos,Negros,Azules)),!.

ubicar_resto_azulejos(3,Cant_azulejos,Rojos,Amarillos,Blancos,Negros,Azules):-
        Blancos_Actualizado is Blancos+Cant_azulejos,
        asserta(tapa(Rojos,Amarillos,Blancos_Actualizado,Negros,Azules)),!.

ubicar_resto_azulejos(4,Cant_azulejos,Rojos,Amarillos,Blancos,Negros,Azules):-
        Negros_Actualizado is Negros+Cant_azulejos,
        asserta(tapa(Rojos,Amarillos,Blancos,Negros_Actualizado,Azules)),!.

ubicar_resto_azulejos(5,Cant_azulejos,Rojos,Amarillos,Blancos,Negros,Azules):-
        Azules_Actualizado is Azules+Cant_azulejos,
        asserta(tapa(Rojos,Amarillos,Blancos,Negros,Azules_Actualizado)),!.


vaciar_fila_zona_preparacion(0,Jugador,_,F2,F3,F4,F5):-
        asserta(espacio_preparacion(Jugador,[],F2,F3,F4,F5)).
  
vaciar_fila_zona_preparacion(1,Jugador,F1,_,F3,F4,F5):-
        asserta(espacio_preparacion(Jugador,F1,[],F3,F4,F5)).
  
vaciar_fila_zona_preparacion(2,Jugador,F1,F2,_,F4,F5):-
        asserta(espacio_preparacion(Jugador,F1,F2,[],F4,F5)).
  
vaciar_fila_zona_preparacion(3,Jugador,F1,F2,F3,_,F5):-
        asserta(espacio_preparacion(Jugador,F1,F2,F3,[],F5)).
  
vaciar_fila_zona_preparacion(4,Jugador,F1,F2,F3,F4,_):-
        asserta(espacio_preparacion(Jugador,F1,F2,F3,F4,[])).

/*posicion en el muro en el que se coloco un azulejo*/
actualizar_puntuacion_muro(Jugador,Pos_Fila,Pos_Columna):-
        muro(Jugador,Tablero),
        nth0(Pos_Fila,Tablero,Fila),
  
        recorrer_fila_hacia_derecha(Fila,Pos_Columna,Pos_Columna,Puntuacion_hacia_derecha),
  
        recorrer_fila_hacia_izquierda(Fila,Pos_Columna,Pos_Columna,Puntuacion_hacia_izquierda),
  
        extraer_columna(Tablero,Pos_Columna,Columna),
        recorrer_fila_hacia_derecha(Columna,Pos_Fila,Pos_Fila,Puntuacion_hacia_abajo),
        recorrer_fila_hacia_izquierda(Columna,Pos_Fila,Pos_Fila,Puntuacion_hacia_arriba),
  
        Puntuacion_por_filas is Puntuacion_hacia_derecha+Puntuacion_hacia_izquierda+1,
  
        Puntuacion_por_columnas is Puntuacion_hacia_abajo+Puntuacion_hacia_arriba+1,
  
        puntuacion(Jugador,Puntuacion),
        Puntuacion_actualizada is Puntuacion + Puntuacion_por_columnas+Puntuacion_por_filas,
        retract(puntuacion(Jugador,Puntuacion)),
        asserta(puntuacion(Jugador,Puntuacion_actualizada)).



/*funcion que recorre la fila en el muro hacia la izquierda para actualizar la cantidad de puntos a sumar*/
recorrer_fila_hacia_izquierda(_,_,-1,0):-!.
recorrer_fila_hacia_izquierda(Fila,Pos_Columna,_,Puntuacion):-
        nth0(Pos_Columna,Fila,Elemento),
        Elemento==0,
        Puntuacion = 0.
recorrer_fila_hacia_izquierda(Fila,Pos_Columna,Pos_Columna,Puntuacion):-
        Pos_Columna_anterior is Pos_Columna - 1,
        recorrer_fila_hacia_izquierda(Fila,Pos_Columna,Pos_Columna_anterior,Puntuacion).
recorrer_fila_hacia_izquierda(Fila,Pos_Columna,Pos_actual,Puntuacion):-
        Pos_actual_anterior is Pos_actual - 1,
        recorrer_fila_hacia_izquierda(Fila,Pos_Columna,Pos_actual_anterior,P1),
        Puntuacion is P1 + 1.

/*funcion que recorre la fila en el muro hacia la derecha para actualizar la cantidad de puntos a sumar*/
recorrer_fila_hacia_derecha(_,_,5,0):-!.
recorrer_fila_hacia_derecha(Fila,_,Pos_actual,Puntuacion):-
        nth0(Pos_actual,Fila,Elemento),
        Elemento==0,
        Puntuacion = 0.
recorrer_fila_hacia_derecha(Fila,Pos_Columna,Pos_Columna,Puntuacion):-
        Pos_Columna_sgt is Pos_Columna + 1,
        recorrer_fila_hacia_derecha(Fila,Pos_Columna,Pos_Columna_sgt,Puntuacion).
recorrer_fila_hacia_derecha(Fila,Pos_Columna,Pos_actual,Puntuacion):-
        Pos_actual_sgt is Pos_actual + 1,
        recorrer_fila_hacia_derecha(Fila,Pos_Columna,Pos_actual_sgt,P1),
        Puntuacion is P1 + 1.

/*verifica si hay una fila completa en el muro */
fila_completa_espacio_preparacion(Fila,Fila_actual,Fila_completa):-
        length(Fila,Len),
        Len=:=Fila_actual+1,
        Fila_completa='True'.
fila_completa_espacio_preparacion(_,_,'False').

/*ubica un azulejo en el muro en la posicion correspondiente segun el color que sea*/
ubicar_Azulejo_Muro(Color,Jugador, Fila_Actual,Position):-
        muro(Jugador,Tablero),
        posicion_color_muro(Fila_Actual,Position,Color),
        nth0(Fila_Actual,Tablero,Fila_a_modificar),
        crear_fila_actualizada_muro(Fila_Actual,Fila_a_modificar,0,Position,[],Fila_Actualizada),
        insertar_tablero_actualizado(Tablero,Fila_Actual,Fila_Actualizada,Tablero_Actualizado),
        retract(muro(Jugador,Tablero)),
        asserta(muro(Jugador,Tablero_Actualizado)).

insertar_tablero_actualizado(Tablero_boleano,0,Fila_modificada,Tablero_booleano_actualizado):-
        nth0(1,Tablero_boleano,F2),
        nth0(2,Tablero_boleano,F3),
        nth0(3,Tablero_boleano,F4),
        nth0(4,Tablero_boleano,F5),
        Tablero_booleano_actualizado =[Fila_modificada,F2,F3,F4,F5].
insertar_tablero_actualizado(Tablero_boleano,1,Fila_modificada,Tablero_booleano_actualizado):-
        nth0(0,Tablero_boleano,F1),
        nth0(2,Tablero_boleano,F3),
        nth0(3,Tablero_boleano,F4),
        nth0(4,Tablero_boleano,F5),
        Tablero_booleano_actualizado =[F1,Fila_modificada,F3,F4,F5].
insertar_tablero_actualizado(Tablero_boleano,2,Fila_modificada,Tablero_booleano_actualizado):-
        nth0(0,Tablero_boleano,F1),
        nth0(1,Tablero_boleano,F2),
        nth0(3,Tablero_boleano,F4),
        nth0(4,Tablero_boleano,F5),
        Tablero_booleano_actualizado =[F1,F2,Fila_modificada,F4,F5].
insertar_tablero_actualizado(Tablero_boleano,3,Fila_modificada,Tablero_booleano_actualizado):-
        nth0(0,Tablero_boleano,F1),
        nth0(1,Tablero_boleano,F2),
        nth0(2,Tablero_boleano,F3),
        nth0(4,Tablero_boleano,F5),
        Tablero_booleano_actualizado =[F1,F2,F3,Fila_modificada,F5].
insertar_tablero_actualizado(Tablero_boleano,4,Fila_modificada,Tablero_booleano_actualizado):-
        nth0(0,Tablero_boleano,F1),
        nth0(1,Tablero_boleano,F2),
        nth0(2,Tablero_boleano,F3),
        nth0(3,Tablero_boleano,F4),
        Tablero_booleano_actualizado =[F1,F2,F3,F4,Fila_modificada].


/*funcion que actualiza una fila en el muro*/
crear_fila_actualizada_muro(_,Fila,Pos_actual,_,Fila_en_Proceso,Fila_Actualizada):-
        length(Fila, Len),
        Pos_actual == Len,
        Fila_Actualizada = Fila_en_Proceso.
crear_fila_actualizada_muro(Pos_fila,Fila,Pos_Actual,Pos_a_modificar,Fila_en_Proceso,Fila_Actualizada):-
        Pos_Actual==Pos_a_modificar,
        posicion_color_muro(Pos_fila,Pos_a_modificar,Valor),
        add(Valor, Fila_en_Proceso, Z1),
        Pos_Actual_sgt is Pos_Actual+1,
        crear_fila_actualizada_muro(Pos_fila,Fila,Pos_Actual_sgt,Pos_a_modificar,Z1,Fila_Actualizada).
crear_fila_actualizada_muro(Pos_fila,Fila,Pos_Actual,Pos_a_modificar,Fila_en_Proceso,Fila_Actualizada):-
        nth0(Pos_Actual,Fila,Elemento),
        add(Elemento,Fila_en_Proceso,Z1),
        Pos_Actual_sgt is Pos_Actual+1,
        crear_fila_actualizada_muro(Pos_fila,Fila,Pos_Actual_sgt,Pos_a_modificar,Z1,Fila_Actualizada).

/*FASE III*/
fase_III(Jugadores,Losetas):-
        format("Inicio de la fase III ~n"),
        llenar_Losetas(Losetas),!,
        suma_losetas(Losetas,Total),
        losetas_vacias_para_proxima_partida(Total,Jugadores,Losetas).
        

losetas_vacias_para_proxima_partida(Total,Jugadores,_):-
        Total==0,
        fin_partida(Jugadores).
losetas_vacias_para_proxima_partida(_,Jugadores,Losetas):-
        desarrollo_de_la_Partida(Jugadores,Losetas).

/*funcion que actualiza la cantidad de puntos a sumar segun la cantidad de azulejos adyacentes*/
suma_losetas([],0):-!.
suma_losetas([L1|Resto],Total):-
    loseta(L1,Az),
    nth0(0,Az,Az1),
    nth0(1,Az,Az2),
    nth0(2,Az,Az3),
    nth0(3,Az,Az4),
    suma_losetas(Resto,Total1),
    Total is Total1 + Az1 + Az2 + Az3 + Az4,!.
  
/*funcion que devuelve los jugadores ganadores*/
jugadores_ganadores([J1|Resto],Jugadores):-
    puntuacion_maxima([J1|Resto],Punt_max),
    jugadores_con_puntuacion_maxima([J1|Resto],Punt_max,[],Jugadores_con_punt),
    length(Jugadores_con_punt,Len),
    Len==1,
    Jugadores =Jugadores_con_punt.
jugadores_ganadores([J1|Resto],Jugadores):-
    cantidad_filas_llenas_maxima([J1|Resto],Cant_max),
    jugadores_con_filas_llenas_maxima([J1|Resto],Cant_max,[],Jugadores_ganadores),
    Jugadores = Jugadores_ganadores.

/*funcion que busca los jugadores con puntuacion maxima*/
jugadores_con_puntuacion_maxima([],_,Z1,Z1):-!.
jugadores_con_puntuacion_maxima([J1|Resto],Punt_max,Lista_en_proceso,Jugadores):-
        puntuacion(J1,Puntuacion_jugador_actual),
        Puntuacion_jugador_actual==Punt_max,
        add(J1,Lista_en_proceso,Lista_en_proceso_act),
        jugadores_con_puntuacion_maxima(Resto,Punt_max,Lista_en_proceso_act,Jugadores).
jugadores_con_puntuacion_maxima([J1|Resto],Punt_max,Lista_en_proceso,Jugadores):-
        jugadores_con_puntuacion_maxima(Resto,Punt_max,Lista_en_proceso_act,Jugadores).
/*funcion que busca la puntuacion maxima entre todos los jugadores*/
puntuacion_maxima([],0):-!.
puntuacion_maxima([J1|Resto],Punt_max):-
        puntuacion(J1,Puntuacion_jugador_actual),
        puntuacion_maxima(Resto,Punt_max_sgt),
        actualizar_puntuacion_maxima(Puntuacion_jugador_actual,Punt_max_sgt,Punt_max).
actualizar_puntuacion_maxima(Punt_jug_act,Punt_max_sgt,Punt_max):-
        Punt_jug_act>Punt_max_sgt,
        Punt_max = Punt_jug_act.
actualizar_puntuacion_maxima(_,Punt_max_sgt,Punt_max_sgt).

/*funcion que busca la cantidad total de filas llenas*/
cantidad_filas_llenas_maxima([],0):-!.
cantidad_filas_llenas_maxima([J1|Resto],Cant_max):-
        cantidad_filas_completadas(J1,Cant_filas_jugador_actual),
        cantidad_filas_llenas_maxima(Resto,Cant_max_sgt),
        actualizar_cantidad_filas_llenas_max(Cant_filas_jugador_actual,Cant_max_sgt,Cant_max).

actualizar_cantidad_filas_llenas_max(Cant_filas_jugador_actual,Cant_max_sgt,Cant_max):-
        Cant_filas_jugador_actual>Cant_max_sgt,
        Cant_max = Cant_filas_jugador_actual.

/*funcion que busca los jugadores que tienen filas llenas*/
jugadores_con_filas_llenas_maxima([],_,Z1,Z1):-!.
jugadores_con_filas_llenas_maxima([J1|Resto],Filas_llenas_max,Lista_en_proceso,Jugadores):-
        cantidad_filas_completadas(J1,Cant_filas_jugador_actual),
        Cant_filas_jugador_actual==Filas_llenas_max,
        add(J1,Lista_en_proceso,Lista_en_proceso_act),
        jugadores_con_filas_llenas_maxima(Resto,Filas_llenas_max,Lista_en_proceso_act,Jugadores_act),
        Jugadores = Jugadores_act.
/*OK*/
fin_partida([]):-!.
fin_partida([J1|Resto]):-

        format("Puntuacion del Jugador ~a ~n",[J1]),
        muro(J1,Tablero),
        /*Puntuacion por las columnas*/
        puntuacion_total_por_las_columnas(Tablero,0,Puntuacion_por_las_columnas),
        format("Puntuacion por las columnas ~n"),
        write(Puntuacion_por_las_columnas),
        format("~n"),

        cantidad_filas_completadas(J1,Cant),
        retract(cantidad_filas_completadas(Jugador,Cant)),
        asserta(cantidad_filas_completadas(Jugador,0)),

        /*Puntuacion por las filas*/
        puntuacion_total_por_las_filas(J1,Tablero,0,Puntuacion_por_las_filas),
        format("Puntuacion por las filas ~n"),
        write(Puntuacion_por_las_filas),
        format("~n"),
  
        /*Puntuacion por color azulejo*/
        puntuacion_por_color_azulejo(1,Tablero,Puntuacion_azulejo_rojo),
        format("Puntuacion por azulejo de color Rojo ~n"),
        write(Puntuacion_azulejo_rojo),
        format("~n"),
        puntuacion_por_color_azulejo(2,Tablero,Puntuacion_azulejo_amarillo),
        format("Puntuacion por azulejo de color Amarillo ~n"),
        write(Puntuacion_azulejo_amarillo),
        format("~n"),
        puntuacion_por_color_azulejo(3,Tablero,Puntuacion_azulejo_blanco),
        format("Puntuacion por azulejo de color Blanco ~n"),
        write(Puntuacion_azulejo_blanco),
        format("~n"),
        puntuacion_por_color_azulejo(4,Tablero,Puntuacion_azulejo_negro),
        format("Puntuacion por azulejo de color Negro ~n"),
        write(Puntuacion_azulejo_negro),
        format("~n"),
        puntuacion_por_color_azulejo(5,Tablero,Puntuacion_azulejo_azul),
        format("Puntuacion por azulejo de color Azul ~n"),
        write(Puntuacion_azulejo_azul),
        format("~n"),
  
        puntuacion(J1,Puntuacion),
        Puntuacion_actualizada is Puntuacion + Puntuacion_azulejo_rojo+Puntuacion_azulejo_amarillo+Puntuacion_azulejo_blanco+Puntuacion_azulejo_negro+Puntuacion_azulejo_azul+Puntuacion_por_las_columnas+Puntuacion_por_las_filas,
        retract(puntuacion(J1,Puntuacion)),
        asserta(puntuacion(J1,Puntuacion_actualizada)),
        format("Puntuacion total del Jugador ~a ~n",[J1]),
        write(Puntuacion_actualizada),
        format("~n"),
        fin_partida(Resto).

puntuacion_total_por_las_columnas(_,5,0):-!.

puntuacion_total_por_las_columnas(Tablero,Pos_actual,Puntuacion):-
        extraer_columna(Tablero,Pos_actual,Colum),
        fila_o_columna_Completa(Colum,Columna_llena),
        Columna_llena=='True',
        Pos_actual_sgt is Pos_actual+1,
        puntuacion_total_por_las_columnas(Tablero,Pos_actual_sgt,Puntuacion_sgt),
        Puntuacion is Puntuacion_sgt + 7.

puntuacion_total_por_las_columnas(Tablero,Pos_actual,Puntuacion):-
        Pos_actual_sgt is Pos_actual+1,
        puntuacion_total_por_las_columnas(Tablero,Pos_actual_sgt,Puntuacion).

puntuacion_total_por_las_filas(_,_,5,0):-!.

puntuacion_total_por_las_filas(Jugador,Tablero,Pos_actual,Puntuacion):-
        nth0(Pos_actual,Tablero,Fila),
        fila_o_columna_Completa(Fila,Fila_llena),
        Fila_llena=='True',

        cantidad_filas_completadas(Jugador,Cant),
        Cant_act is Cant+1,
        retract(cantidad_filas_completadas(Jugador,Cant)),
        asserta(cantidad_filas_completadas(Jugador,Cant_act)),

        Pos_actual_sgt is Pos_actual+1,
        puntuacion_total_por_las_filas(Jugador,Tablero,Pos_actual_sgt,Puntuacion_sgt),
        Puntuacion is Puntuacion_sgt + 2.

puntuacion_total_por_las_filas(Jugador,Tablero,Pos_actual,Puntuacion):-
        Pos_actual_sgt is Pos_actual+1,
        puntuacion_total_por_las_filas(Jugador,Tablero,Pos_actual_sgt,Puntuacion).

puntuacion_azulejo(Fila_0,Fila_1,Fila_2,Fila_3,Fila_4,Puntuacion):-
        Fila_0=='True',
        Fila_1=='True',
        Fila_2=='True',
        Fila_3=='True',
        Fila_4=='True',
        Puntuacion=10.
puntuacion_azulejo(_,_,_,_,_,0).

puntuacion_por_color_azulejo(1,Tablero,Puntuacion):-
        azulejo_color_en_Muro(Tablero,0,2,Fila_0),
        azulejo_color_en_Muro(Tablero,1,3,Fila_1),
        azulejo_color_en_Muro(Tablero,2,4,Fila_2),
        azulejo_color_en_Muro(Tablero,3,0,Fila_3),
        azulejo_color_en_Muro(Tablero,4,1,Fila_4),
        puntuacion_azulejo(Fila_0,Fila_1,Fila_2,Fila_3,Fila_4,Puntuacion).

puntuacion_por_color_azulejo(2,Tablero,Puntuacion):-
        azulejo_color_en_Muro(Tablero,0,1,Fila_0),
        azulejo_color_en_Muro(Tablero,1,2,Fila_1),
        azulejo_color_en_Muro(Tablero,2,3,Fila_2),
        azulejo_color_en_Muro(Tablero,3,4,Fila_3),
        azulejo_color_en_Muro(Tablero,4,0,Fila_4),
        puntuacion_azulejo(Fila_0,Fila_1,Fila_2,Fila_3,Fila_4,Puntuacion).

puntuacion_por_color_azulejo(3,Tablero,Puntuacion):-
        azulejo_color_en_Muro(Tablero,0,4,Fila_0),
        azulejo_color_en_Muro(Tablero,1,0,Fila_1),
        azulejo_color_en_Muro(Tablero,2,1,Fila_2),
        azulejo_color_en_Muro(Tablero,3,2,Fila_3),
        azulejo_color_en_Muro(Tablero,4,3,Fila_4),
        puntuacion_azulejo(Fila_0,Fila_1,Fila_2,Fila_3,Fila_4,Puntuacion).

puntuacion_por_color_azulejo(4,Tablero,Puntuacion):-
        azulejo_color_en_Muro(Tablero,0,3,Fila_0),
        azulejo_color_en_Muro(Tablero,1,4,Fila_1),
        azulejo_color_en_Muro(Tablero,2,0,Fila_2),
        azulejo_color_en_Muro(Tablero,3,1,Fila_3),
        azulejo_color_en_Muro(Tablero,4,2,Fila_4),
        puntuacion_azulejo(Fila_0,Fila_1,Fila_2,Fila_3,Fila_4,Puntuacion).
      
puntuacion_por_color_azulejo(5,Tablero,Puntuacion):-
        azulejo_color_en_Muro(Tablero,0,0,Fila_0),
        azulejo_color_en_Muro(Tablero,1,1,Fila_1),
        azulejo_color_en_Muro(Tablero,2,2,Fila_2),
        azulejo_color_en_Muro(Tablero,3,3,Fila_3),
        azulejo_color_en_Muro(Tablero,4,4,Fila_4),
        puntuacion_azulejo(Fila_0,Fila_1,Fila_2,Fila_3,Fila_4,Puntuacion).
  
/*funcion que verifica si la casilla determinada esta llena en el muro */
azulejo_color_en_Muro(Tablero,Pos_fila,Pos_columna,Fila,Casilla_llena):-
        nth0(Pos_fila,Tablero,Fila),
        nth0(Pos_columna,Fila,Elem),
        Elem=\=0,
        Casilla_llena='True'.
azulejo_color_en_Muro(_,_,_,'False').
/*_ es Pos*/
fila_o_columna_Completa(Fila,Fila_o_columna_completa):-
        nth0(0,Fila,Elem_pos_0),
        nth0(1,Fila,Elem_pos_1),
        nth0(2,Fila,Elem_pos_2),
        nth0(3,Fila,Elem_pos_3),
        nth0(4,Fila,Elem_pos_4),
        
        Elem_pos_0=\=0,
        Elem_pos_1=\=0,
        Elem_pos_2=\=0,
        Elem_pos_3=\=0,
        Elem_pos_4=\=0,
        Fila_o_columna_completa='True'.
fila_o_columna_Completa(_,'False').

fila_Muro_Completada([],'False'):-!.
fila_Muro_Completada([J1|_],Fila_llena_general):-
        muro(J1,Tablero_boleano),
        filas_completa_Muro(Tablero_boleano,0,Fila_llena_bool),
        Fila_llena_bool=='True',
        Fila_llena_general='True',!.
       
fila_Muro_Completada([_|Resto],Fila_llena_general):-
        fila_Muro_Completada(Resto,Fila_llena_general),!.

filas_completa_Muro(_,5,'False'):-!.
filas_completa_Muro(Tablero,Pos_fila_actual,Fila_actual_llena):-
        nth0(Pos_fila_actual,Tablero,Fila),
        fila_o_columna_Completa(Fila,Fila_actual_llena_sgt),
        Fila_actual_llena_sgt=='True',
        Fila_actual_llena='True',!.
        
filas_completa_Muro(Tablero,Pos_fila_actual,Fila_actual_llena):-
        Pos_fila_sgt is Pos_fila_actual +1,
        filas_completa_Muro(Tablero,Pos_fila_sgt,Fila_actual_llena),!.
