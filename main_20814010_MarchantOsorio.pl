%-----------------------------------------------------------------------------------
%-----------------------------------LAB 2: PROLOG-----------------------------------
%-----------------------------------------------------------------------------------
% Nombre: Cristóbal Marchant Osorio
% Rut: 20.814.010-8
% Seccion: A-1
% Profesor de seccion: Roberto Gonzales Ibanez

%-----------------------------------------------------------------------------------
%-----------------------------------TDA-CARDSSET------------------------------------
%-----------------------------------------------------------------------------------
/*
Dominio:
enteros-> 	Nelementos,J,K,I,Seed,Nmax,Num
list->		Lsalida,Lsimbolos,Lentrada,Laux
atom->		Str


Predicados:
c1(Nelementos,Lsalida,Lsimbolos).
op(Nelementos,J,K,Num).
c2(Nelementos,Lsalida,J,K).
c3(Nelementos,Lsalidad,Lsimbolos).
c4(Nelementos,I,J,Lsalida,Lsimbolos).
c5(Nelementos,Lsalida,I,J,K,Lsimbolos).
c6(Nelementos,I,J,K,Lsalida,Lsimbolos).
azar(Lentrada,Seed,Lsalida).
azarito(Lentrada,Seed,Laux,Lsalida).
cardsSet2(Nelementos,Lsalida,Lsimbolos).
cardsSet(Lsimbolos,Nelementos,Nmax,Seed,Lsalida).
tamListIgual(Lentrada).
eleRep(Lentrada).
oneElemPerCard(Lentrada).
oneElemPerCard2(Lentrada,Lentrada).
cardsSetIsDobble(Lentrada).
cardsSetNthCard(Lsalida,Num,Lentrada).
cardsSetFindTotalCards(Lentrada,Num).
cardsSetMissingCards(Lentrada,Lsalida).
cardsSetToString(Lentrada,Str).


Metas:
	Secundarias:
	c1(Nelementos,Lsalida,Lsimbolos).
	op(Nelementos,J,K,Num).
	c2(Nelementos,Lsalida,J,K).
	c3(Nelementos,Lsalidad,Lsimbolos).
	c4(Nelementos,I,J,Lsalida,Lsimbolos).
	c5(Nelementos,Lsalida,I,J,K,Lsimbolos).
	c6(Nelementos,I,J,K,Lsalida,Lsimbolos).
	azar(Lentrada,Seed,Lsalida).
    azarito(Lentrada,Seed,Laux,Lsalida).
	cardsSet2(Nelementos,Lsalida,Lsimbolos).
	tamListIgual(Lentrada).
	eleRep(Lentrada).
	oneElemPerCard(Lentrada).
	oneElemPerCard2(Lentrada,Lentrada).
	cardsSetToString2(Lentrada,Str,Laux,Num).	

	Primarias:
	cardsSet(Lsimbolos,Nelementos,Nmax,Seed,Lsalida).
	cardsSetIsDobble(Lentrada).
	cardsSetNthCard(Lentrada,Num,Lsalida).
	cardsSetFindTotalCards(Lentrada,Num).
	cardsSetMissingCards(Lentrada,Lsalida).
	cardsSetToString(Lentrada,Str).
*/
/*
--------------------------Representacion--------------------------------------------
El TDA-CardsSet se reperesenta a traves de una lista, que contine un número 
determinado de cartas, siendo este modificado para entregar cartas hasta cierto 
punto y poder dar aleatorización al orden de estas cartas.
------------------------------------------------------------------------------------
*/




% Lista de elementos planteada a utilizar para maximo 8 elementos.
% [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,as,at,au,av,aw,ax,ay,az,ba,bb,bc,bd,be,bf]

%----------------------Generacion de 1 Carta----------------------------------------
% Constructor
% Dominio: entero X entero X list
% Descripcion: recibe un numero y una Lista de simbolos y genera la primera carta
c1(N,M,Ls):-nth1(1,Ls,Xsimbo),N1 is N+1, c1(N,Xsimbo,M,2,N1,Ls).

c1(_,M,[M],N1,N1,_).
c1(N,M,[M|L],K,N1,Ls):-
    N>=K,
    nth1(K,Ls,M2),
    K1 is K+1,
    c1(N,M2,L,K1,N1,Ls).

%----------------------Generacion de N Cartas---------------------------------------
% Dominio: entero X entero X entero X entero
% Descripcion: realiza una operacion matematica con los números
op(N, J, K, M):- M is (N*J)+K+1.

%-----------------------------------------------------------------------------------
% Constructor
% Dominio: entero X list X entero X list
% Descripcion: recibe 2 números y una Lista de simbolos y genera una carta
c2(N,M,J,Ls):-op(N, J, 1, M2),nth1(M2,Ls,Xsimbo),N1 is N+1, c2(N,Xsimbo,M,J,2,N1,Ls).

c2(_,M,[M],_,N1,N1,_).
c2(N,M,[M|L],J,K,N1,Ls):-
    N>=K,
    op(N, J, K, M2),
    nth1(M2,Ls,Xsimbo),
    K1 is K+1,
    c2(N,Xsimbo, L, J, K1,N1,Ls).

%-----------------------------------------------------------------------------------
% Constructor
% Dominio: entero X list X entero
% Descripcion: recibe un número y una Lista de simbolos y genera las n primera cartas
c3(N,R,Ls):-c2(N,M,1,Ls),N1 is N+1,nth1(1,Ls,Xsimbo),c3(N,[Xsimbo|M],R,2,N1,Ls).

c3(_,R,[R],N1,N1,_). 
c3(N,R,[R|C],J,N1,Ls):-
    N>=J,
    c2(N,L,J,Ls),
    J1 is J+1,
    nth1(1,Ls,Xsimbo),
    c3(N,[Xsimbo|L],C,J1,N1,Ls).

%----------------------Generacion de N*N Cartas-------------------------------------
% Dominio: entero X entero X entero X entero X entero
% Descripcion: realiza una operacion matematica con los números
op2(N,I,J,K,L):- L is (N+2+(N*(K-1))+((((I-1)*(K-1))+J-1) mod N)).

%-----------------------------------------------------------------------------------
% Constructor
% Dominio: entero X entero X entero X Lista X Lista 
% Descripcion: genera una carta
c4(N,I,J,M,Ls):-op2(N,I,J,1,M2),N1 is N+1,nth1(M2,Ls,Xsimbo),c4(N,Xsimbo,M,I,J,2,N1,Ls).

c4(_,M,[M],_,_,N1,N1,_).
c4(N,M,[M|L],I,J,K,N1,Ls):-
    N>=K,
    op2(N,I,J,K,M2),
    nth1(M2,Ls,Xsimbo),
    K1 is K+1,
    c4(N,Xsimbo,L,I,J,K1,N1,Ls).

%-----------------------------------------------------------------------------------
% Constructor
% Dominio: entero X lista X entero X entero X entero X lista
% Descripcion: genera n cartas
c5(N,R,I,J,K,Ls):-c4(N,I,J,M,Ls),N1 is N+1,Iz is I+1,nth1(Iz,Ls,Xsimbo),c5(N,[Xsimbo|M],R,I,2,N1,K,Ls).

c5(_,R,[R],_,N1,N1,_,_). 
c5(N,R,[R|C],I,J,N1,K,Ls):-
    N>=J,
    c4(N,I,J,M,Ls),
    J1 is J+1,
    Iz is I+1,
    nth1(Iz,Ls,Xsimbo),
    c5(N,[Xsimbo|M],C,I,J1,N1,K,Ls).

%-----------------------------------------------------------------------------------
% Constructor
% Dominio: entero X entero X entero X entero X Lista X Lista 
% Descripcion: genera todas las n*n cartas
c6(N,I,J,K,R,Ls):- c5(N,M,I,J,K,Ls), N1 is N+1,c6(N,2,2,J,N1,M,R,Ls).

c6(_,N1,_,_,N1,R,R,_).
c6(N,I,K,J,N1,M,R,Ls):-
    N>=I,
    c5(N,M1,I,J,K,Ls),
    I1 is I+1,
    append(M,M1,Z),
    c6(N,I1,K,J,N1,Z,R,Ls).

%----------------------Predicado de Azar--------------------------------------------
% Modificador
% Dominio: Lista X Enetero X Lista
% Descripcion: predicado que permite randomizar una lista
azar(L,Seed,LOut):- azarito(L,Seed,[],LOut).

% Modificador
% Dominio: Lista X Enetero X Lista X Lista
% Descripcion: predicado que permite randomizar una lista
% Caso 1, SeedAux < N
azarito(Lista,Seed,L,LOut):-
    length(Lista, N),
    not(N=0),
    SeedAux is Seed mod N,
    SeedAux < N,
    nth0(SeedAux, Lista, L1),
    delete(Lista,L1,L11),
    append(L, [L1], L2),
    azarito(L11,Seed,L2,LOut),
    !.
% Caso 2, SeedAux >= N, aumenta el SeedAux en 1
azarito(Lista,Seed,L,LOut):-
    length(Lista, N),
    not(N=0),
    SeedAux is Seed mod N,
    not(SeedAux < N),
    Seed1 is Seed+1,
    azarito(Lista,Seed1,L,LOut),
    !.
% Caso 3, Lista vacia
azarito(Lista,_,L,L):-
    length(Lista, N),
    N=0,
    !.          

%----------------------Generacion del mazo-----------------------------------------
% Constructor
% Dominio: Enetero X Lista X Lista
% Descripcion: Crea el cardsSet sin MaxE y sin random
cardsSet2(N,R,Ls):-
    c1(N,R1,Ls),
    N1 is N-1,
    c3(N1,R2,Ls),
    c6(N1,1,1,1,R3,Ls),
    append([R1],R2,R4),
    append(R4,R3,R).

% Constructor
% Dominio: Lista X Enetero X Entero X Entero X Lista 
% Descripcion: Crea el cardsSet completo bajo los parametros ingresados
% Caso 1, MaxE = Variante
cardsSet(Lsimbo,N,NC,Seed,Lsalida):-			
    cardsSet2(N,R,Lsimbo),
    length(R,S),
    S=NC,
    cardsSet2(N,Lsalida1,Lsimbo),
    azar(Lsalida1,Seed,Lsalida),
    !.
% Caso 2, MaxE = Numero
cardsSet(Lsimbo,N,NC,Seed,Lsalida):-
    cardsSet2(N,R,Lsimbo),
    length(R,S),
    S > NC,
    length(Lsalida1, NC),				% para cortar las cartas
    append(Lsalida1, _,R),
    azar(Lsalida1,Seed,Lsalida),
    !.

%--------------------------cardsSetIsDobble----------------------------------------
% Pertinencia
% Dominio: Lista
% Descripcion: predicado que ve si todas las cartas tiene el mismo tamaño
% Caso 1, Se acaban las Cartas
tamListIgual([_|[]]):-!.			
% Caso 2, verifica y avanza
tamListIgual([E|Cola]):-			
    nth1(1,Cola,M),
    same_length(E,M),
    tamListIgual(Cola).

% Pertinencia
% Dominio: Lista
% Descripcion: elementos repetidos en una carta
% Caso 1, Cuando queda solo 1 carta por revisar
eleRep([E|[]]):-
    length(E,M),
    list_to_set(E,E1),
    length(E1, M1),
    M==M1,
    !.
% Caso 2, Verifica carta por carta
eleRep([E|Cola]):-				% este se verifica cuando se aplica el anterior
    length(E,M),
    list_to_set(E,E1),
    length(E1, M1),
    M==M1,
    eleRep(Cola).

% Pertinencia
% Dominio: Lista
% Descripcion: Un elemento en común de una lista
% Caso 1, Cuando la lista es vacia
oneElemPerCard([]).				
% Caso 2, comprueba 1 cartas con todas las demas, luego la siguiente carta.
oneElemPerCard([E|Cola]):-oneElemPerCard2([E|Cola], E), oneElemPerCard(Cola).

% Pertinencia
% Dominio: Lista X Lista
% Descripcion: Un elemento en común entre una carta y una lista
% Caso 1, Cuando la siguiente carta es nula
oneElemPerCard2([_|[]],_):-!.	
% Caso 2, Compara una 2 cartas y avanza	
oneElemPerCard2([_|Cola],N):-		
    nth1(1,Cola,M),					
    intersection(N,M,L),			
    length(L,1),
    oneElemPerCard2(Cola,N).

% Pertinencia
% Dominio: Lista
% Descripcion: Comprueba si un mazo es valido
cardsSetIsDobble(L):-
    tamListIgual(L),
    eleRep(L),
    oneElemPerCard(L).

%----------------------------cardsSetNthCard----------------------------------------
% Selector
% Dominio: Lista X Entero X Lista
% Descripcion: Regrega La carta N, desde un mazo, entre 0 y tamaño mazo-1
cardsSetNthCard(Le, N, Ls):-
    nth0(N,Le, Ls).

%-----------------------cardsSetFindTotalCards--------------------------------------
% Otros
% Dominio: Lista X Entero
% Descripcion: Cantidad de cartas totales, a base de una carta
% Caso Lista con elementos
cardsSetFindTotalCards(Card, S):-
    length(Card,N),
    N>0,
    S is ((N-1)*(N-1))+N,
    !.
% Caso Lista vacia
cardsSetFindTotalCards(Card, 0):-
    length(Card,N),
    N=0.

%---------------------cardsSetMissingCards------------------------------------------
% Otros
% Dominio: Lista X Lista
% Descripcion: Entrega las cartas faltantes del set.
cardsSetMissingCards(Cs,MC):-	%tomar en cuenta que siempre se trabajara con una lista del tipo [a,b,c,d,e,f]
    nth1(1, Cs, R1),
    length(R1, S),
    cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,as,at,au,av,aw,ax,ay,az,ba,bb,bc,bd,be,bf],S,_,0,Out),
    subtract(Out, Cs, MC). 

%---------------------cardsSetToString---------------------------------------------
% Otra
% Dominio: Lista X Lista
% Descripcion: entrega el mazo en string
cardsSetToString(CS,S):- cardsSetToString2(CS, S2,[],1),reverse(S2,S3),atomics_to_string(S3,"\n",S).

% Modificador
% Dominio: Lista X Str X Lista X Entero
% Descripcion: transforma cada carta en un str
% Caso 1, Lista vacia
cardsSetToString2([],Str,Str,_).
% Caso 2, Carta por Carta.
cardsSetToString2([C|Cola],Str,L,I):-
    atomics_to_string(C,"-",X),
    append(["carta "],[I],Cartita),
    atomics_to_string(Cartita," ",Cartita2),
    append([Cartita2],[X],X1),
    atomics_to_string(X1,": ",X2),
    I1 is I+1,
    cardsSetToString2(Cola,Str,[X2|L],I1).

%-----------------------------------------------------------------------------------
%-----------------------------------TDA-PLAYERS-------------------------------------
%-----------------------------------------------------------------------------------
/*
Dominio:
enteros-> 	Num
list->		Lsalida,Laux,Lentrada
atom->		Name


Predicados:
genPlayers(Num,Lsalida,Laux).
showTurno(Lentrada,Num).
showName(Lentrada,Name).


Metas:
	Secundarias:
	showTurno(Lentrada,Num).
	showName(Lentrada,Name).

	Primarias:
	genPlayers(Num,Lsalida,Laux).
*/
/*
--------------------------Representacion--------------------------------------------
El TDA-Players se representa a traves de una lista que contiene el nombre del 
jugadore, el turno actual de este, el puntaje actual y un espacio por si se 
requerie en algun modo de juego adicional.
------------------------------------------------------------------------------------
*/
% Constructor
% Dominio: Entero X Lista X Lista
% Descripcion: Obtener el perfil de cada usuario
% Caso 1, No mas usuarios
genPlayers(1,Lp,Lp):-!.
% Caso 2, Generar usuarios
genPlayers(N,Lp,X):-			  %tendra el nombre del jugador, el turno, los puntos, mazo actual.
    append([N],[0,0,[]],Player),
    N1 is N-1,
    genPlayers(N1,Lp,[Player|X]).

% Pertinencia
% Dominio: Lista X Lista
% Descripcion: entrega el turno 
showTurno([_,Turno,_,_], Turno).   

% Pertinencia
% Dominio: Lista X Lista
% Descripcion: Entrega el nombre
showName([Name,_,_,_],Name).
%-----------------------------------------------------------------------------------
%-----------------------------------TDA-GAME----------------------------------------
%-----------------------------------------------------------------------------------
/*
Dominio:
enteros-> 	Num,Seed
list->		Lsalida,Laux,Lsalida,LPlayers,Action
atom->		Name,Str,Action


Predicados:
dobbleGame(Num,Lentrada,Str,Seed,Lsalida).
dobbleGameRegister(Name,Lentrada,Lsalida).
turnito(LPlayers,Name,Name).
dobbleGameWhoseTurnIsIt(Lentrada,Name).
dobbleGamePlay(Lentrada,Action,Lsalida).
mayorAmenor(Lentrada,Laux,Num,Lsalida).
puesto(Lentrada,Lsalida).
primerPuesto(Lentrada,Num,Laux,Lsalida).
jugadoresCola(Lentrada,Laux,Lsalida).
dobbleGameStatus(Lentrada,Str).
dobbleGameScore(Lentrada,Name,Num).
jugadoresToStr(Lentrada,Laux,Lsalida).
dobbleGameToString(Lentrada,Lsalida).

Metas:
	Secundarias:
	turnito(LPlayers,Name,Name).
	mayorAmenor(Lentrada,Laux,Num,Lsalida).
	puesto(Lentrada,Lsalida).
	primerPuesto(Lentrada,Num,Laux,Lsalida).
	jugadoresCola(Lentrada,Laux,Lsalida).
	jugadoresToStr(Lentrada,Laux,Lsalida).

	Primarias:
	dobbleGame(Num,Lentrada,Str,Seed,Lsalida).
	dobbleGameRegister(Name,Lentrada,Lsalida).
	dobbleGameWhoseTurnIsIt(Lentrada,Name).
	dobbleGamePlay(Lentrada,Action,Lsalida).
	dobbleGameStatus(Lentrada,Str).
	dobbleGameScore(Lentrada,Name,Num).
	dobbleGameToString(Lentrada,Str).
*/
/*
--------------------------Representacion--------------------------------------------
El TDA-Game se representa a traves de una lista compuesta por Numero de jugadores,
la lista de jugadres, la mesa actual, el mazo de cartas, y el modo de juego.
------------------------------------------------------------------------------------
*/
%----------------------------dobbleGame---------------------------------------------
% Constructor
% Dominio: Entero X Lista X Str X Entero X Lista
% Descripcion: crea el juego base
dobbleGame(NPlayers,CS,Mode,Seed,Game):-		 %[NumPlayers,ListPlayers,Mesa,CardsSet,Modo]
    genPlayers(NPlayers,Lp,[[1,0,0,[]]]),
    azar(Lp,Seed,Lp2),							 %azar actua sobre la lista de jugadores
    reverse(CS, CS1),
    append([[]],CS1,CS2),
    reverse(CS2,CS3),
    append([NPlayers,[Lp2]],[[],CS3, Mode],Game).
%-----------------------dobbleGameRegister------------------------------------------
% Modificador
% Dominio: String X Lista X Lista
% Descripcion: agrega al jugador ingresado en el juego
%              tira false en caso de ingresar mas personas de lo permitido y 
%              en caso de que una persona este repetida
dobbleGameRegister(Name,[N,[Lp],[],Cs,Mode],[N1,[GameOut1],[],Cs,Mode]):-	%recibe un name, game, gameOut
    not(select([Name,0,0,[]],Lp,[Name,0,0,[]],GameOut1)),					%para cuando este el nombre repetido
    select([N,0,0,[]],Lp,[Name,0,0,[]],GameOut1),							%agregar el name
    number(N),
    N1 is N-1,
    !.

%-----------------------dobbleGameWhoseTurnIsIt-------------------------------------
% Pertinencia
% Dominio: Lista X Str X Str
% Descripcion: busca a quien esta de turno
% Caso 1, cuando la lista acaba y entrega el primer turno
turnito([_|[]],Acum,Acum).			
% Caso 2, cuando encuentra el turno
turnito([X|Cola],Nombre,_):-		
    showTurno(X,T1),
    nth1(1,Cola,N1),
    showTurno(N1,T2),
    T1>T2,
    showName(N1,Nombre).
% Caso 3, que busca el turno
turnito([X|Cola],Name,Acum):-
    showTurno(X,T1),
    nth1(1,Cola,N1),
    showTurno(N1,T2),
    T1=T2,
    turnito(Cola,Name,Acum).

% Pertinencia
% Dominio: Lista X Str
% Descripcion: Entrega a la persona de turno
dobbleGameWhoseTurnIsIt([_,[Pl],_,_,_],Name):-
    nth1(1,Pl, Felem),
    showName(Felem,Name1),
    turnito(Pl,Name,Name1),
    !.

%--------------------------------dobbleGamePlay-------------------------------------
%-----------------------------------------------------------------------------------
%---------solo se pretende el play para el stack mode -> "stackMode".---------------
%-----------------------------------------------------------------------------------
% Pertinencia
% Dominio: Lista X Atom X Lista
% Descripcion: Modifica el Game segun la "Action que sea ingresada"
% Caso Null
dobbleGamePlay([A,[B],_,Cs,Mode],Action,[A,[B],Mesa2,Acortar,Mode]):-	
    Mode="stackMode",
    Action=null,
    length(Mesa2,2),
    append(Mesa2,Acortar,Cs),
    !.
% Caso Pass
dobbleGamePlay([A,[B],Mesa,Cs,Mode],Action,[A,[B1],Mesa,Cs,Mode]):-
    Mode="stackMode",
    Action=[pass],
    dobbleGameWhoseTurnIsIt([A,[B],Mesa,Cs,Mode], Name),
    select([Name,T,P,[]],B,_,B),
    T1 is T+1,
    select([Name,_,_,[]],B,[Name,T1,P,[]],B1),
    !.
% Caso spotIt 1, queda 1 carta
dobbleGamePlay([A,[B],[_,[]],_,Mode],Action,Out):- 						
    Mode="stackMode",
    Action=[spotIt,_,_],
    dobbleGamePlay([A,[B],[_,[]],_,Mode],[finish],Out),
    !.
% Caso spotIt 2, ya no hay mas cartas
dobbleGamePlay([A,[B],[[]],_,Mode],Action,Out):- 						
    Mode="stackMode",
    Action=[spotIt,_,_],
    dobbleGamePlay([A,[B],[_,[]],_,Mode],[finish],Out),
    !.
% Caso spotIt 3, correcto
dobbleGamePlay([A,[B],[E1,E2],Cs,Mode],Action,Gout):-					
    Mode="stackMode",
    Action=[spotIt,Nombre,Elemento],
    dobbleGameWhoseTurnIsIt([A,[B],[E1,E2],Cs,Mode], Nombre1),	
    Nombre=Nombre1,														%ve si el que juega esta de turno
    intersection(E1,E2,[Elemento]),										%ve respuesta buena o mala
    select([Nombre,T,P,[]],B,_,B),										%encuentra el nombre
    T1 is T+1,															%suma 1 turno
    P1 is P+1,															%suma 1 punto
    select([Nombre,_,_,[]],B,[Nombre,T1,P1,[]],B1),
    dobbleGamePlay([A,[B1],[E1,E2],Cs,Mode],null,Gout),					%siguientes 2 cartas
    !.
% Caso spotIt 4, incorrecto
dobbleGamePlay([A,[B],[E1,E2],Cs,Mode],Action,[A,[B1],[E1,E2],Cs,Mode]):- 
    Mode="stackMode",
    Action=[spotIt,Nombre,Elemento],
    dobbleGameWhoseTurnIsIt([A,[B],[E1,E2],Cs,Mode], Nombre1),	
    Nombre=Nombre1,														%ve si el que juega esta de turno
    not(intersection(E1,E2,[Elemento])),
    select([Nombre,T,P,[]],B,_,B),
    T1 is T+1,															%suma 1 turno
    select([Nombre,_,_,[]],B,[Nombre,T1,P,[]],B1),
    !.
% Caso finish
dobbleGamePlay([A,[B],_,_,Mode],Action, [S2,[B],[fin],[fin],"finish"]):-
    Mode="stackMode",
    Action=[finish],
    mayorAmenor([A,[B],_,_,Mode],[],0,S1),
    puestos(S1,S2),
    !.

% Otro
% Dominio: Lista X Lista X Entero X Lista
% Descripcion: predicado que ordena a los jugaodres segun su puntaje
% Caso 1, Lista Vacia
mayorAmenor([_,[[]],_,_,_],Lmm,I,[[I]|Lmm]):-!.		
% Caso 2, encuentra el puntaje menor y vuelve ver si hay otro igual
mayorAmenor([_,[Pl],_,_,_],Lmm,I,R):-
    select([Name,_,I,[]],Pl,_,Pl),
    append([[Name,I]],Lmm,Lmm1),        % agrega a Lnueva
    delete(Pl,[Name,_,I,[]],Pl2),       % elimina de Lvieja
    mayorAmenor([_,[Pl2],_,_,_],Lmm1,I,R),
    !.
% Caso 3, aumenta el puntaje en 1 y vuelve a buscar
mayorAmenor([_,[Pl],_,_,_],Lmm,I,R):-
    not(select([_,_,I,[]],Pl,_,Pl)),
    I1 is I+1,
    mayorAmenor([_,[Pl],_,_,_],Lmm,I1,R),
    !.
%----------------------------
% Otro
% Dominio: Lista X Lista
% Descripcion: ve si es ganador o empate y perdedor
puestos([[X]|Cola],Salida):-primerPuesto(Cola,X,[],Salida).

% Otro
% Dominio: Lista X Entero X Lista X Lista
% Descripcion: predicado que ordena a los jugaodres segun su puntaje
% Caso 1, compara el puntaje maximo con el del usuario
primerPuesto([First|Cola],X,R,Rf):-
    nth1(1,First,Name),
    nth1(2,First,Punto),
    X=Punto,
    append(R,[Name],R1),
    primerPuesto(Cola,X,R1,Rf),
    !.
% Caso 2, El puntaje era menor, entonces agrega a pierde
primerPuesto([First|Cola],X,R,Rf):-
    nth1(2,First,Punto),
    not(X=Punto),
    jugadoresCola([First|Cola],[],Pierde),
    append(["perdedor/xs:"],Pierde,S1),
    append(R,S1,S2),
    append(["ganador/xs:"],S2,Rf),
    !.
% Caso 3, Todos tenian los mismos puntos
primerPuesto([],_,R,Rf):-
    append(["Empate entre:"],R,Rf).

% Otro
% Dominio: Lista X Lista X Lista
% Descripcion: encuentra al resto de jugadores(perdedores)
% Caso 1, si hay perdedores
jugadoresCola([[X|_]|Cola],L,LS):-
    append([X],L,L1),
    jugadoresCola(Cola,L1,LS),
    !.
% Caso 2, no hay perdedores
jugadoresCola([],LS,LS).

%------------------------------dobbleGameStatus-------------------------------------
% Otro
% Dominio: Lista X Str
% Descripcion: entrega el status del juego
% Caso 1, En progreso
dobbleGameStatus([_,_,_,_,Mode],"En progreso"):-
    not(Mode="finish"),
    !.
% Caso 2, Finalizado
dobbleGameStatus([_,_,_,_,Mode],"Finalizado"):-
    Mode="finish".
%------------------------------dobbleGameScore--------------------------------------
% Selector
% Dominio: Lista X Str X Entero
% Descripcion: Entrega el puntaje de un jugadores 
dobbleGameScore([_,[Pl],_,_,_], Name, Score):-	
    select([Name,_,Score,[]],Pl,_,Pl),
    !.
%------------------------------dobbleGameToString-----------------------------------
% Otro
% Dominio: Lista X Lista X Lista
% Descripcion: Predicado que obtiene una lista con los jugadores y sus datos
% Caso 1, no vacio
jugadoresToStr([[Name,Turno,Puntos,_]|Cola],L,LS):-
    atomics_to_string(["jugador/a:",Name,"Posee:",Puntos,"Puntos","en el turno:",Turno]," ",JugadoresStr),
    append([JugadoresStr],L,L1),
    jugadoresToStr(Cola,L1,LS),
    !.
% Caso 2, Vacio
jugadoresToStr([],LS,LS).

% Otro
% Dominio: Lista X Str
% Descripcion: Predicado que obtiene un String del juego ingresado
% Caso 1, Juego en progreso
dobbleGameToString([NumPlayers,[Pl],Mesa,Mazo,Modo],GameStr):-					%Para juego en desarrollo							
    number(NumPlayers),
    cardsSetToString(Mesa,MesaStr),
    cardsSetToString(Mazo,MazoStr),
    jugadoresToStr(Pl,[],Jugadores),
    atomics_to_string(Jugadores,"\n",JugadoresStr),
    append([],["Jugando:",Modo,"- activo\n","En la Mesa tenemos:",MesaStr,"\n",
                "En el mazo restante se tienen:" ,MazoStr,"\n","El estado de jugadores es:",
                JugadoresStr],GameStr1),
    atomics_to_string(GameStr1," ",GameStr),
    !.
% Caso 2, Juego finalizado                    
dobbleGameToString([NumPlayers,[Pl],_,_,Modo],GameStr):-					%para juego finalizado
    Modo="finish",
    jugadoresToStr(Pl,[],Jugadores),
    atomics_to_string(Jugadores,"\n",JugadoresStr),
    append(["Modo de juego finalizado\n","Lo que nos deja los siguientes puntajes:",
             JugadoresStr,"\n","El resultado final fue:"], NumPlayers, GameStr1),
    atomics_to_string(GameStr1," ",GameStr).

%-----------------------------------------------------------------------------------
%-----------------------------------EJMEPLOS----------------------------------------
%-----------------------------------------------------------------------------------

%-----------------------------------------------------------------------------------
%-----------------------------------TDA-cardsSet------------------------------------
%-----------------------------------------------------------------------------------

%------------------------------cardsSet-Constructor---------------------------------
% CardsSet de 3 elementos por carta, con un maximo de 4 cartas, de orden aleatorio.
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],3,4,4512331,CS1).

% CardsSet de 4 elementos por carta, con un maximo de 8 cartas, sin orden aleatorio.
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,8,0,CS2).

% CardsSet de 4 elementos por carta, que muestra todas las cartas, de orden aleatorio.
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,MAX,4352315,CS3).

%--------------------------cardsSetIsDobble----------------------------------------
% Dobble de un mazo de 3 elementos y maximo 4 cartas con orden aleatorio -> true
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n],3,4,4512331,CS1), cardsSetIsDobble(CS1). 

% Dobble de un mazo de 3 elementos y maximo 3 cartas -> false (hay mas de un elemento en común)
% cardsSetIsDobble([[1,2,3],[1,4,5],[1,5,6]]).      

% Dobble de un mazo de 3 elementos y maximo 3 cartas -> false (hay 2 elementos iguales en 1 carta)
% cardsSetIsDobble([[1,2,3],[1,4,5],[1,6,6]]).   

%----------------------------cardsSetNthCard----------------------------------------
% Primera carta de un mazo de 3 elementos, max 4 cartas y desordenado
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],3,4,4512331,CS1), cardsSetNthCard(CS1,0,Nth1).

% Primera carta de un mazo de 3 elementos, max 4 cartas y ordenado
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],3,4,0,CS2), cardsSetNthCard(CS2,0,Nth2).

% Quinta carta de un mazo de 4 elementos, max 5 cartas y desordenado
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,235236,CS3), cardsSetNthCard(CS3,4,Nth3).

%-----------------------cardsSetFindTotalCards--------------------------------------
% Cartas totales de la primera carta de un mazo de 3 elementos, max 4 cartas y desordenado
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],3,4,4512331,CS1), cardsSetNthCard(CS1,0,Nth1), cardsSetFindTotalCards(Nth1, TCards).

% Cartas totales de una carta valida
% cardsSetFindTotalCards([1,3,4,5], TCards2).

% Cartas totales de la quinta carta de un mazo de 4 elementos, max 5 cartas y desordenado
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,235236,CS3), cardsSetNthCard(CS3,4,Nth3), cardsSetFindTotalCards(Nth3, TCards3).

%---------------------cardsSetMissingCards------------------------------------------
% missingCards de un mazo de 3 elementos, max 4 cartas y desordenado
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],3,4,4512331,CS1), cardsSetMissingCards(CS1, Mcards1).

% missingCards de una carta de 8 elementos
% cardsSetMissingCards([[a,b,c,d,e,f,g,h]], TCards2).

% missingCards de un mazo de 4 elementos, max 5 cartas y ordenada
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), cardsSetMissingCards(CS3, Mcards1).

%---------------------cardsSetToString---------------------------------------------
% cardsSetToString de un mazo de 3 elementos, max 4 cartas y desordenado
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],3,4,4512331,CS1), cardsSetToString(CS1,CSTS1).

% cardsSetToString de una carta
% cardsSetToString([[a,b,c,d,e]],CSTS2).

% cardsSetToString de un mazo de 4 elementos, max 5 cartas y ordenada
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), cardsSetToString(CS3,CSTS3).

%-----------------------------------------------------------------------------------
%-----------------------------------TDA-Game----------------------------------------
%-----------------------------------------------------------------------------------

%---------------------------------Game-Constructor----------------------------------
% dobbleGame con 4 jugadores, stackMode y un mazo de 3 elementos, max 4 cartas y desordenado
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],3,4,4512331,CS1), dobbleGame(4,CS1,"stackMode",5234,G1).

% dobbleGame con 3 jugadores, stackMode y un mazo de 3 elementos, con el max de cartas y ordenado
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],3,MAX,0,CS2), dobbleGame(2,CS2,"stackMode",1324,G2).

% dobbleGame con 2 jugadores, stackMode y un mazo de 4 elementos, max 5 cartas y ordenado
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), dobbleGame(2,CS3,"stackMode",6312,G3).

%-----------------------dobbleGameRegister------------------------------------------
% register "user1" -> lo agrega
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), dobbleGame(2,CS3,"stackMode",6312,G3),dobbleGameRegister("user1",G3,G4).

% register "user1" -> register "user1" -> false (porque ya se encuentra "user1" registrado)
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), dobbleGame(2,CS3,"stackMode",6312,G3),dobbleGameRegister("user1",G3,G4),dobbleGameRegister("user1",G4,G5).

% register "user1" -> register "user2" -> lo agrega
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), dobbleGame(2,CS3,"stackMode",6312,G3),dobbleGameRegister("user1",G3,G4),dobbleGameRegister("user2",G4,G5).

%-----------------------dobbleGameWhoseTurnIsIt-------------------------------------
% juego en progreso action->pass
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), dobbleGame(2,CS3,"stackMode",6312,G3),dobbleGameRegister("user1",G3,G4),dobbleGameRegister("user2",G4,G5),dobbleGamePlay(G5,null,G6), dobbleGamePlay(G6,[pass],G7),dobbleGameWhoseTurnIsIt(G7,Name).

% juego en progreso action->spotIt->correcto
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), dobbleGame(2,CS3,"stackMode",6312,G3),dobbleGameRegister("user1",G3,G4),dobbleGameRegister("user2",G4,G5),dobbleGamePlay(G5,null,G6), dobbleGamePlay(G6,[pass],G7),dobbleGamePlay(G7,[spotIt,"user2",a],G8),dobbleGameWhoseTurnIsIt(G8,Name).

% juego en progreso action->spotIt->incorrecto
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), dobbleGame(2,CS3,"stackMode",6312,G3),dobbleGameRegister("user1",G3,G4),dobbleGameRegister("user2",G4,G5),dobbleGamePlay(G5,null,G6), dobbleGamePlay(G6,[pass],G7),dobbleGamePlay(G7,[spotIt,"user2",a],G8),dobbleGamePlay(G8,[spotIt,"user1",b],G9),dobbleGameWhoseTurnIsIt(G9,Name).

%--------------------------------dobbleGamePlay-------------------------------------
% action -> null
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), dobbleGame(2,CS3,"stackMode",6312,G3),dobbleGameRegister("user1",G3,G4),dobbleGameRegister("user2",G4,G5),dobbleGamePlay(G5,null,G6).

% action -> [pass] -> turno+1
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), dobbleGame(2,CS3,"stackMode",6312,G3),dobbleGameRegister("user1",G3,G4),dobbleGameRegister("user2",G4,G5),dobbleGamePlay(G5,null,G6), dobbleGamePlay(G6,[pass],G7).

% action -> [spotIt,user,Element] -> correct -> turno+1 punto+1
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), dobbleGame(2,CS3,"stackMode",6312,G3),dobbleGameRegister("user1",G3,G4),dobbleGameRegister("user2",G4,G5),dobbleGamePlay(G5,null,G6), dobbleGamePlay(G6,[pass],G7),dobbleGamePlay(G7,[spotIt,"user2",a],G8).

% action -> [spotIt,user,Element] -> incorrect -> turno+1
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), dobbleGame(2,CS3,"stackMode",6312,G3),dobbleGameRegister("user1",G3,G4),dobbleGameRegister("user2",G4,G5),dobbleGamePlay(G5,null,G6), dobbleGamePlay(G6,[pass],G7),dobbleGamePlay(G7,[spotIt,"user2",a],G8),dobbleGamePlay(G8,[spotIt,"user1",b],G9).

% action -> [finish]
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), dobbleGame(2,CS3,"stackMode",6312,G3),dobbleGameRegister("user1",G3,G4),dobbleGameRegister("user2",G4,G5),dobbleGamePlay(G5,null,G6), dobbleGamePlay(G6,[pass],G7),dobbleGamePlay(G7,[spotIt,"user2",a],G8),dobbleGamePlay(G8,[spotIt,"user1",b],G9), dobbleGamePlay(G9,[finish], Gdiez).

%------------------------------dobbleGameStatus-------------------------------------
% Juego en progreso -> action -> pass
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), dobbleGame(2,CS3,"stackMode",6312,G3),dobbleGameRegister("user1",G3,G4),dobbleGameRegister("user2",G4,G5),dobbleGamePlay(G5,null,G6), dobbleGamePlay(G6,[pass],G7),dobbleGameStatus(G7,Status1).

% Juego finalizado
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), dobbleGame(2,CS3,"stackMode",6312,G3),dobbleGameRegister("user1",G3,G4),dobbleGameRegister("user2",G4,G5),dobbleGamePlay(G5,null,G6), dobbleGamePlay(G6,[pass],G7),dobbleGamePlay(G7,[spotIt,"user2",a],G8),dobbleGamePlay(G8,[spotIt,"user1",b],G9), dobbleGamePlay(G9,[finish], Gdiez),dobbleGameStatus(Gdiez,Status2).

% Juego en progreso -> action -> spotIt -> incorrecto
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), dobbleGame(2,CS3,"stackMode",6312,G3),dobbleGameRegister("user1",G3,G4),dobbleGameRegister("user2",G4,G5),dobbleGamePlay(G5,null,G6), dobbleGamePlay(G6,[pass],G7),dobbleGamePlay(G7,[spotIt,"user2",a],G8),dobbleGamePlay(G8,[spotIt,"user1",b],G9),dobbleGameStatus(G9,Status3).

%------------------------------dobbleGameScore--------------------------------------
% Juego en progreso
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), dobbleGame(2,CS3,"stackMode",6312,G3),dobbleGameRegister("user1",G3,G4),dobbleGameRegister("user2",G4,G5),dobbleGamePlay(G5,null,G6), dobbleGamePlay(G6,[pass],G7),dobbleGamePlay(G7,[spotIt,"user2",a],G8),dobbleGamePlay(G8,[spotIt,"user1",b],G9),dobbleGameScore(G9,"user1",Score1).

% Juego finalizado
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), dobbleGame(2,CS3,"stackMode",6312,G3),dobbleGameRegister("user1",G3,G4),dobbleGameRegister("user2",G4,G5),dobbleGamePlay(G5,null,G6), dobbleGamePlay(G6,[pass],G7),dobbleGamePlay(G7,[spotIt,"user2",a],G8),dobbleGamePlay(G8,[spotIt,"user1",b],G9), dobbleGamePlay(G9,[finish], Gdiez),dobbleGameScore(Gdiez,"user2",Score2).

% Juego en progreso
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), dobbleGame(2,CS3,"stackMode",6312,G3),dobbleGameRegister("user1",G3,G4),dobbleGameRegister("user2",G4,G5),dobbleGamePlay(G5,null,G6), dobbleGamePlay(G6,[pass],G7),dobbleGamePlay(G7,[spotIt,"user2",a],G8),dobbleGamePlay(G8,[spotIt,"user1",b],G9),dobbleGameScore(G9,"user2",Score3).

%------------------------------dobbleGameToString-----------------------------------
% Juego en progreso
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), dobbleGame(2,CS3,"stackMode",6312,G3),dobbleGameRegister("user1",G3,G4),dobbleGameRegister("user2",G4,G5),dobbleGamePlay(G5,null,G6), dobbleGamePlay(G6,[pass],G7),dobbleGamePlay(G7,[spotIt,"user2",a],G8),dobbleGamePlay(G8,[spotIt,"user1",b],G9),dobbleGameToString(G9,Gstr1).

% Juego finalizado
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), dobbleGame(2,CS3,"stackMode",6312,G3),dobbleGameRegister("user1",G3,G4),dobbleGameRegister("user2",G4,G5),dobbleGamePlay(G5,null,G6), dobbleGamePlay(G6,[pass],G7),dobbleGamePlay(G7,[spotIt,"user2",a],G8),dobbleGamePlay(G8,[spotIt,"user1",b],G9), dobbleGamePlay(G9,[finish], Gdiez),dobbleGameToString(Gdiez,Gstr2).

% Juego en progreso
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],4,5,0,CS3), dobbleGame(2,CS3,"stackMode",6312,G3),dobbleGameRegister("user1",G3,G4),dobbleGameRegister("user2",G4,G5),dobbleGamePlay(G5,null,G6), dobbleGamePlay(G6,[pass],G7),dobbleGameToString(G7,Gstr3).
