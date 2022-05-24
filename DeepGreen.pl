/* Para comenzar el sistema, escribe 'start.'
 Deep Green
 David Emmanuel González Cázares
 Nuria Hernández Alás
 Josué Doménico Chicatti Avendaño
*/

%Se invoca el módulo de jpl para usar herramientas visuales de Java en ProLog
:- use_module(library(jpl)).

%El método principal para empezar el Sistema Experto Deep Green
start :-sleep(0.4),	
		write('-----------------------------------------------------------------'),nl,
		sleep(0.4),
		write('*****************************************************************'),nl,
		sleep(0.2),
		write("###################||| SISTEMA EXPERTO DEEP GREEN |||#########################"),nl,
		sleep(0.4),
		write('*****************************************************************'),nl,
		sleep(0.4),
		write('-----------------------------------------------------------------'),nl,nl,nl,

		interface2.
        
%La base de conocimientos de distintos síntomas que existen, formulados como predicados para hacer preguntas que verifiquen si padece el síntoma
        
    sintoma(Paciente,fiebre) :- verificar(Paciente," tiene fiebre (y/n) ?").
 
    sintoma(Paciente,sarpullido) :- verificar(Paciente," tiene sarpullido (y/n) ?").
  
    sintoma(Paciente,dolor_de_cabeza) :- verificar(Paciente," tiene dolor_de_cabeza (y/n) ?").

    sintoma(Paciente,escurrimiento_nasal) :- verificar(Paciente," tiene escurrimiento_nasal (y/n) ?").
    
    sintoma(Paciente,conjunctivitis) :- verificar(Paciente," tiene conjunctivitis (y/n) ?").
    
    sintoma(Paciente,tos) :- verificar(Paciente," tiene tos (y/n) ?").
	
    sintoma(Paciente,dolor_corporal) :- verificar(Paciente," tiene dolor_corporal (y/n) ?").
 
    sintoma(Paciente,escalofrios) :- verificar(Paciente," tiene escalofrios (y/n) ?").
   
    sintoma(Paciente,garganta_rasposa) :- verificar(Paciente," tiene garganta_rasposa (y/n) ?").
  
    sintoma(Paciente,estornudos) :- verificar(Paciente," tiene estornudos (y/n) ?").
   
    sintoma(Paciente,gangleos_inflamados) :- verificar(Paciente," tiene gangleos_inflamados (y/n) ?").

    sintoma(Paciente,perdida_de_olfato):-verificar(Paciente," tiene perdida_de_olfato (y/n) ?").

    sintoma(Paciente,febricula):-verificar(Paciente," tiene febricula (y/n) ?").

    sintoma(Paciente,dolor_de_pecho):-verificar(Paciente," dolor_de_pecho ?").

    sintoma(Paciente,flema):-verificar(Paciente," tiene flema (y/n) ?").

    sintoma(Paciente,asfixia):-verificar(Paciente," tiene asfixia (y/n) ?").

    sintoma(Paciente,tos_con_sangre):-verificar(Paciente," tiene tos_con_sangre (y/n) ?").

    sintoma(Paciente,nausea):-verificar(Paciente," tiene nausea (y/n) ?").

    sintoma(Paciente,dificultad_inhalando):-verificar(Paciente," tiene dificultad_inhalando (y/n) ?").

    sintoma(Paciente,taquicardia):-verificar(Paciente," tiene taquicardia (y/n) ?").

    sintoma(Paciente,cansancio):-verificar(Paciente," tiene cansancio (y/n) ?").

    sintoma(Paciente,sudoracion):-verificar(Paciente," tiene sudoracion (y/n) ?").

    sintoma(Paciente,acidez):-verificar(Paciente," tiene acidez (y/n) ?").

    sintoma(Paciente,disfagia):-verificar(Paciente," tiene disfagia (y/n) ?").
    
    sintoma(Paciente,dolor_muscular):-verificar(Paciente," tiene dolor_muscular (y/n) ?").

    sintoma(Paciente,dolor_abdominal):-verificar(Paciente," tiene dolor_abdominal (y/n) ?").
    
    sintoma(Paciente,inflamacion_abdominal):-verificar(Paciente," tiene inflamacion_abdominal (y/n) ?").

    sintoma(Paciente,dolor_estomacal):-verificar(Paciente," tiene dolor_estomacal (y/n) ?").

    sintoma(Paciente,vomito):-verificar(Paciente," tiene vomito (y/n) ?").
    
    sintoma(Paciente,nudo_en_la_garganta):-verificar(Paciente," tiene nudo_en_la_garganta (y/n) ?").

    sintoma(Paciente,evacuaciones_acuosas):-verificar(Paciente," tiene evacuaciones_acuosas (y/n) ?").

    sintoma(Paciente,inapetencia):-verificar(Paciente," tiene inapetencia (y/n) ?").

    sintoma(Paciente,adelgazamiento):-verificar(Paciente," tiene adelgazamiento (y/n) ?").

    sintoma(Paciente,picazon_de_piel):-verificar(Paciente," tiene picazon_de_piel (y/n) ?").

    sintoma(Paciente,evacuaciones_con_sangre):-verificar(Paciente," tiene evacuaciones_con_sangre (y/n) ?").

    sintoma(Paciente,incomodidad_al_beber):-verificar(Paciente," tiene incomodidad_al_beber (y/n) ?").

    sintoma(Paciente,dolores_prolongados):-verificar(Paciente," tiene dolores_prolongados (y/n) ?").

    sintoma(Paciente,malestar_desde_hace_tiempo):-verificar(Paciente," tiene malestar_desde_hace_tiempo (y/n) ?").

    sintoma(Paciente,gases):-verificar(Paciente," tiene gases (y/n) ?").

    sintoma(Paciente,dolor_anal):-verificar(Paciente," tiene dolor_anal (y/n) ?").

    sintoma(Paciente,sangrado_anal):-verificar(Paciente," tiene sangrado_anal (y/n) ?").

    sintoma(Paciente,fragilidad_de_cabello):-verificar(Paciente," tiene fragilidad_de_cabello (y/n) ?").

    sintoma(Paciente,deshidratacion):-verificar(Paciente," tiene deshidratacion (y/n) ?").

    sintoma(Paciente,evacuaciones_con_sangre):-verificar(Paciente," tiene evacuaciones_con_sangre (y/n) ?").

    sintoma(Paciente,piel_amarilla):-verificar(Paciente," tiene piel_amarilla (y/n) ?").

    sintoma(Paciente,dolor_garganta):-verificar(Paciente," tiene dolor_garganta (y/n) ?").

    sintoma(Paciente,labios_partidos):-verificar(Paciente," tiene labios_partidos (y/n) ?").

    sintoma(Paciente,lengua_blanquecina):-verificar(Paciente," tiene lengua_blanquecina (y/n) ?").

    sintoma(Paciente,afonia):-verificar(Paciente," tiene afonia (y/n) ?").

    sintoma(Paciente,problema_de_ronquidos):-verificar(Paciente," tiene problema_de_ronquidos (y/n) ?").

    sintoma(Paciente,espasmos_musculares):-verificar(Paciente," tiene espasmos_musculares (y/n) ?").

    sintoma(Paciente,espasmos_de_cuello):-verificar(Paciente," tiene espasmos_de_cuello (y/n) ?").

%Las reglas, presentadas en forma y orden de árbol de decisiones, que determinan cuál diagnóstico de enfermedad se cumple según las respuestas a los síntomas

    diagnostico(Paciente,david19):-
	Paciente = 'David'.

    diagnostico(Paciente,covid19):-
	sintoma(Paciente,fiebre),
	sintoma(Paciente,tos),
	sintoma(Paciente,fatiga),
	sintoma(Paciente,escurrimiento_nasal),
	sintoma(Paciente,perdida_de_olfato).
        
    diagnostico(Paciente,sarampion) :-
        sintoma(Paciente,fiebre),
        sintoma(Paciente,tos),
        sintoma(Paciente,conjunctivitis),
        sintoma(Paciente,escurrimiento_nasal),
        sintoma(Paciente,sarpullido).
    
    diagnostico(Paciente,sarampion_aleman) :-
        sintoma(Paciente,fiebre),
        sintoma(Paciente,dolor_de_cabeza),
        sintoma(Paciente,escurrimiento_nasal),
        sintoma(Paciente,sarpullido).

    diagnostico(Paciente,dengue):-
	sintoma(Paciente,fiebre),
	sintoma(Paciente,nausea),
	sintoma(Paciente,sarpullido),
	sintoma(Paciente,dolor_de_cabeza),
	sintoma(Paciente,dolor_muscular).

    diagnostico(Paciente,chikungunya):-
	sintoma(Paciente,fiebre),
	sintoma(Paciente,sarpullido),
	sintoma(Paciente,nausea),
	sintoma(Paciente,conjuntivitis),
	sintoma(Paciente,dolor_cabeza),
	sintoma(Paciente,dolor_muscular),
	sintoma(Paciente,dolor_de_espalda).

    diagnostico(Paciente,zika):-
	sintoma(Paciente,fiebre),
	sintoma(Paciente,sarpullido),
	sintoma(Paciente,nausea),
	sintoma(Paciente,conjuntivitis),
	sintoma(Paciente,dolor_cabeza),
	sintoma(Paciente,dolor_muscular).

    diagnostico(Paciente,bronquitis):-
	sintoma(Paciente,febricula),
	sintoma(Paciente,tos),
	sintoma(Paciente,fatiga),
	sintoma(Paciente,dolor_de_pecho),
	sintoma(Paciente,escalofrios),
	sintoma(Paciente,flema).
/*
    diagnostico(Paciente,cancer_pulmonar):-
	sintoma(Paciente,tos),
	sintoma(Paciente,dolor_de_pecho),
	sintoma(Paciente,fatiga),
	sintoma(Paciente,asfixia),
	sintoma(Paciente,tos_con_sangre).
  */      
    diagnostico(Paciente,influenza) :-
        sintoma(Paciente,fiebre),
        sintoma(Paciente,dolor_de_cabeza),
        sintoma(Paciente,dolor_corporal),
        sintoma(Paciente,conjunctivitis),
        sintoma(Paciente,escalofrios),
        sintoma(Paciente,garganta_rasposa),
        sintoma(Paciente,escurrimiento_nasal),
        sintoma(Paciente,tos).   

    diagnostico(Paciente,neumonia):-
	sintoma(Paciente,fiebre),
	sintoma(Paciente,dolor_de_pecho),
	sintoma(Paciente,fatiga),
	sintoma(Paciente,tos),
	sintoma(Paciente,nausea).

    diagnostico(Paciente,edema_pulmonar):-
	sintoma(Paciente,fatiga),
	sintoma(Paciente,flema),
	sintoma(Paciente,asfixia),
	sintoma(Paciente,dificultad_inhalando),
	sintoma(Paciente,taquicardia).

    diagnostico(Paciente,asma):-
	sintoma(Paciente,dolor_de_pecho),
	sintoma(Paciente,tos),
	sintoma(Paciente,dificultad_inhalando).
        
    diagnostico(Paciente,resfriado_comun) :-
        sintoma(Paciente,dolor_de_cabeza),
        sintoma(Paciente,estornudos),
        sintoma(Paciente,garganta_rasposa),
        sintoma(Paciente,escurrimiento_nasal),
        sintoma(Paciente,escalofrios).

    diagnostico(Paciente,malaria):-
	sintoma(Paciente,fiebre),
	sintoma(Paciente,escalofrios),
	sintoma(Paciente,dolor_de_cabeza),
	sintoma(Paciente,sudoracion).

    diagnostico(Paciente,covid19):-
	sintoma(Paciente,fiebre),
	sintoma(Paciente,garganta_rasposa),
	sintoma(Paciente,cansancio),
	sintoma(Paciente,dolor_de_cabeza).
        
    diagnostico(Paciente,paperas) :-
        sintoma(Paciente,fiebre),
        sintoma(Paciente,gangleos_inflamados).
    
    diagnostico(Paciente,varicela) :-
        sintoma(Paciente,fiebre),
        sintoma(Paciente,escalofrios),
        sintoma(Paciente,dolor_corporal),
        sintoma(Paciente,sarpullido).
    
    diagnostico(Paciente,sarampion) :-
        sintoma(Paciente,tos),
        sintoma(Paciente,estornudos),
        sintoma(Paciente,escurrimiento_nasal).

    diagnostico(Paciente,fiebre_amarilla):-
	sintoma(Paciente,fiebre),
	sintoma(Paciente,dolor_muscular),
	sintoma(Paciente,nausea),
	sintoma(Paciente,vomito),
	sintoma(Paciente,cansancio),
	sintoma(Paciente,piel_amarilla).

    diagnostico(Paciente,reflujo_acido):-
	sintoma(Paciente,acidez),
	sintoma(Paciente,dolor_de_pecho),
	sintoma(Paciente,disfagia),
	sintoma(Paciente,nudo_en_la_garganta).

    diagnostico(Paciente,diarrea):-
	sintoma(Paciente,dolor_abdominal),
	sintoma(Paciente,fiebre),
	sintoma(Paciente,evacuaciones_acuosas).

    diagnostico(Paciente,gastritis):-
	sintoma(Paciente,dolor_estomacal),
	sintoma(Paciente,nausea),
	sintoma(Paciente,vomito),
	sintoma(Paciente,inapetencia).

    diagnostico(Paciente,reflujo_acido):-
	sintoma(Paciente,acidez),
	sintoma(Paciente,dolor_de_pecho),
	sintoma(Paciente,regurgitaciones).

    diagnostico(Paciente,diarrea):-
	sintoma(Paciente,dolor_abdominal),
	sintoma(Paciente,nausea),
	sintoma(Paciente,vomitos),
	sintoma(Paciente,fiebre),
	sintoma(Paciente,evacuaciones_acuosas).

    diagnostico(Paciente,colangitis):-
	sintoma(Paciente,adelgazamiento),
	sintoma(Paciente,inapetencia),
	sintoma(Paciente,fiebre),
	sintoma(Paciente,dolor_abdominal),
	sintoma(Paciente,cansancio),
	sintoma(Paciente,picazon_de_piel).

    diagnostico(Paciente,colitis):-
	sintoma(Paciente,dolor_abdominal),
	sintoma(Paciente,escalofrios),
	sintoma(Paciente,fiebre),
	sintoma(Paciente,evacuaciones_con_sangre).
	
    diagnostico(Paciente,ulcera_gastrica):-
	sintoma(Paciente,dolor_estomacal),
	sintoma(Paciente,incomodidad_al_beber),
	sintoma(Paciente,dolores_prolongados),
	sintoma(Paciente,malestar_desde_hace_tiempo).

    diagnostico(Paciente,intolerancia_alimentaria):-
	sintoma(Paciente,nausea),
	sintoma(Paciente,dolor_abdominal),
	sintoma(Paciente,dolor_de_cabeza),
	sintoma(Paciente,gases).

    diagnostico(Paciente,hepatitis_B):-
	sintoma(Paciente,febricula),
	sintoma(Paciente,inapetencia),
	sintoma(Paciente,fatiga),
	sintoma(Paciente,dolor_muscular),
	sintoma(Paciente,nausea),
	sintoma(Paciente,piel_amarilla).

    diagnostico(Paciente,hepatitis_C):-
	sintoma(Paciente,dolor_abdominal),
	sintoma(Paciente,fatiga),
	sintoma(Paciente,fiebre),
	sintoma(Paciente,inflamacion_abdominal),
	sintoma(Paciente,inapetencia),
	sintoma(Paciente,secrecion_de_orina_oscura).

    diagnostico(Paciente,pancreatitis_aguda):-
	sintoma(Paciente,fiebre),
	sintoma(Paciente,dolor_abdominal),
	sintoma(Paciente,vomito),
	sintoma(Paciente,dolor_de_espalda),
	sintoma(Paciente,inflamacion_abdominal).

    diagnostico(Paciente,intolerancia_alimentaria):-
	sintoma(Paciente,nausea),
	sintoma(Paciente,dolor_abdominal),
	sintoma(Paciente,gases).

    diagnostico(Paciente,hemorroides):-
	sintoma(Paciente,dolor_abdominal),
	sintoma(Paciente,dolor_anal),
	sintoma(Paciente,bultos_anales).

    diagnostico(Paciente,diarrea):-
	sintoma(Paciente,dolor_abdominal),
	sintoma(Paciente,evacuaciones_acuosas).

    diagnostico(Paciente,hepatitis_B):-
	sintoma(Paciente,inapetencia),
	sintoma(Paciente,fatiga),
	sintoma(Paciente,dolor_muscular),
	sintoma(Paciente,espasmos_musculares),
	sintoma(Paciente,malestar_desde_hace_tiempo).

    diagnostico(Paciente,hemorroides):-
    	sintoma(Paciente,dolor_abdominal),
	sintoma(Paciente,dolor_anal),
	sintoma(Paciente,sangrado_anal).

    diagnostico(Paciente,anemia):-
	sintoma(Paciente,cansancio),
	sintoma(Paciente,taquicardia),
	sintoma(Paciente,dificultad_inhalando),
	sintoma(Paciente,fragilidad_de_cabello).

    diagnostico(Paciente,colitis):-
	sintoma(Paciente,dolor_abdominal),
	sintoma(Paciente,deshidratacion),
	sintoma(Paciente,evacuaciones_con_sangre).

    diagnostico(Paciente,hepatitis_B):-
	sintoma(Paciente,inapetencia),
	sintoma(Paciente,fatiga),
	sintoma(Paciente,dolor_muscular),
	sintoma(Paciente,piel_amarilla).

    diagnostico(Paciente,candidiasis_oral):-
	sintoma(Paciente,disfagia),
	sintoma(Paciente,dolor_garganta),
	sintoma(Paciente,labios_partidos),
	sintoma(Paciente,lengua_blanquecina).

    diagnostico(Paciente,laringitis):-
	sintoma(Paciente,tos),
	sintoma(paciente,afonia),
	sintoma(Paciente,problema_de_ronquidos).

    diagnostico(Paciente,tetanos):-
	sintoma(Paciente,disfagia),
	sintoma(Paciente,dolor_muscular),
	sintoma(Paciente,espasmos_musculares),
	sintoma(Paciente,espasmos_de_cuello).
        
	diagnostico(_,"enfermedad. Lo lamento, no puedo detectar la posible enfermedad").
	
%Regla que determina el valor de la respuesta según lo que escribe el usuario en la interface
    response(Respuesta) :-
        read(Respuesta),
        write(Respuesta),nl.
		
%La acción que ejecuta las preguntas para el usuario del sistema
preguntar(Paciente,Pregunta) :-
	write(Paciente),write(', acaso'),write(Pregunta),
	/*read(N),
	( (N == yes ; N == y)
      ->
       assert(yes(Pregunta)) ;
       assert(no(Pregunta)), fail),*/
	
	interface(', acaso',Paciente,Pregunta),
	write('Loading.'),nl,
	sleep(1),
	write('Loading..'),nl,
	sleep(1),
	write('Loading...'),nl,
	sleep(1),
    nl.
	
%Base de conocimientos dinámica para acertar si un síntoma es positivo con "yes" o es negativo como "no". Esto determina el valor de los síntomas para el diagnóstico
:- dynamic yes/1,no/1.		
	
verificar(P,S) :-
   (yes(S) 
    ->
    true ;
    (no(S)
     ->
     fail ;
     preguntar(P,S))).
	 
%Método que borra el conocimiento de síntomas del usuario anterior
undo :- retract(yes(_)),fail. 
undo :- retract(no(_)),fail.
undo.


%Escribe el resultado final con el diagnóstico del sistema experto
pt(Paciente):- 

		diagnostico(Paciente,Enfermedad),
		interface3(Paciente,', usted quizas tiene ',Enfermedad,'.'),
        write(Paciente),write(', usted quizas tiene '),write(Enfermedad),write('.'),undo,end.

end :-
		nl,nl,nl,
		sleep(0.7),
		write('*****************************************************************'),nl,
		sleep(0.4),
		write("################||| GRACIAS |||#####################"),nl,
		sleep(0.4),
		write('*****************************************************************'),nl.

%La interaz para hacer las preguntas de los síntomas. La variable N es la respuesta que pone el usuario para confirmar o negar el síntoma
interface(X,Y,Z) :-
	atom_concat(Y,X, FAtom),
	atom_concat(FAtom,Z,FinalAtom),
	jpl_new('javax.swing.JFrame', ['Sistema Experto'], F),
	jpl_new('javax.swing.JLabel',['--- SISTEMA MEDICO EXPERTO ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [400,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showInputDialog, [F,FinalAtom], N),
	jpl_call(F, dispose, [], _), 
	write(N),nl,
	( (N == yes ; N == y)
      ->
       assert(yes(Z)) ;
       assert(no(Z)), fail).
	   	
%La interfaz para determinar el nombre del usuario y comenzar la consulta
interface2 :-
	jpl_new('javax.swing.JFrame', ['Sistema Experto'], F),
	jpl_new('javax.swing.JLabel',['--- SISTEMA MEDICO EXPERTO ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [400,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showInputDialog, [F,'Hola. ¿Como se encuentra? Primero que nada, por favor dime cual es tu nombre'], N),
	jpl_call(F, dispose, [], _), 
	/*write(N),nl,*/
	(	N == @(null)
		->	write('usted ha cancelado'),interface3('usted ha cancelado. ','Gracias ','por ','usarme.'),end,fail
		;	write("Hola. ¿Como se encuentra? Primero que nada, por favor dime cual es tu nombre : "),write(N),nl,pt(N)
	).
	
	
%Última interfaz que se usa para cuando se obtiene el resultado de la consulta, o se cancela la consulta
interface3(P,W1,D,W2) :-
	atom_concat(P,W1, A),
	atom_concat(A,D,B),
	atom_concat(B,W2,W3),
	jpl_new('javax.swing.JFrame', ['Sistema Experto'], F),
	jpl_new('javax.swing.JLabel',['--- SISTEMA MEDICO EXPERTO ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [400,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showMessageDialog, [F,W3], N),
	jpl_call(F, dispose, [], _), 
	/*write(N),nl,*/
	(	N == @(void)
		->	write('')
		;	write("")
	).
	
%help. da ayuda en caso de no saber las instrucciones de uso del sistema experto
help :- write("Para empezar el sistema experto, por favor escriba 'start.' and presione la llave de Enter").
