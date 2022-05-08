%--------------------------------------------------------------
listSymbols(L):-
    append([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],[aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,as,at,au,av,aw,ax,ay,az,ba,bb,bc],L).
%--------------------------------------------------------------
c1(N,M,Ls):-nth1(1,Ls,Xsimbo),N1 is N+1, c1(N,Xsimbo,M,2,N1,Ls).

c1(_,M,[M],N1,N1,_).
c1(N,M,[M|L],K,N1,Ls):-
    N>=K,
    nth1(K,Ls,M2),
    K1 is K+1,
    c1(N,M2,L,K1,N1,Ls).
%--------------------------------------------------------------
op(N, J, K, M):- M is (N*J)+K+1.
%--------------------------------------------------------------
c2(N,M,J,Ls):-op(N, J, 1, M2),nth1(M2,Ls,Xsimbo),N1 is N+1, c2(N,Xsimbo,M,J,2,N1,Ls).

c2(_,M,[M],_,N1,N1,_).
c2(N,M,[M|L],J,K,N1,Ls):-
    N>=K,
    op(N, J, K, M2),
    nth1(M2,Ls,Xsimbo),
    K1 is K+1,
    c2(N,Xsimbo, L, J, K1,N1,Ls).
%--------------------------------------------------------------
c3(N,R,Ls):-c2(N,M,1,Ls),N1 is N+1,nth1(1,Ls,Xsimbo),c3(N,[Xsimbo|M],R,2,N1,Ls).

c3(_,R,[R],N1,N1,_). 
c3(N,R,[R|C],J,N1,Ls):-
    N>=J,
    c2(N,L,J,Ls),
    J1 is J+1,
    nth1(1,Ls,Xsimbo),
    c3(N,[Xsimbo|L],C,J1,N1,Ls).
%--------------------------------------------------------------
op2(N,I,J,K,L):- L is (N+2+(N*(K-1))+((((I-1)*(K-1))+J-1) mod N)).
%--------------------------------------------------------------
c4(N,I,J,M,Ls):-op2(N,I,J,1,M2),N1 is N+1,nth1(M2,Ls,Xsimbo),c4(N,Xsimbo,M,I,J,2,N1,Ls).

c4(_,M,[M],_,_,N1,N1,_).
c4(N,M,[M|L],I,J,K,N1,Ls):-
    N>=K,
    op2(N,I,J,K,M2),
    nth1(M2,Ls,Xsimbo),
    K1 is K+1,
    c4(N,Xsimbo,L,I,J,K1,N1,Ls).
%--------------------------------------------------------------
c5(N,R,I,J,K,Ls):-c4(N,I,J,M,Ls),N1 is N+1,Iz is I+1,nth1(Iz,Ls,Xsimbo),c5(N,[Xsimbo|M],R,I,2,N1,K,Ls).

c5(_,R,[R],_,N1,N1,_,_). 
c5(N,R,[R|C],I,J,N1,K,Ls):-
    N>=J,
    c4(N,I,J,M,Ls),
    J1 is J+1,
    Iz is I+1,
    nth1(Iz,Ls,Xsimbo),
    c5(N,[Xsimbo|M],C,I,J1,N1,K,Ls).
%--------------------------------------------------------------
c6(N,I,J,K,R,Ls):- c5(N,M,I,J,K,Ls), N1 is N+1,c6(N,2,2,J,N1,M,R,Ls).

c6(_,N1,_,_,N1,R,R,_).
c6(N,I,K,J,N1,M,R,Ls):-
    N>=I,
    c5(N,M1,I,J,K,Ls),
    I1 is I+1,
    append(M,M1,Z),
    c6(N,I1,K,J,N1,Z,R,Ls).
%--------------------------------------------------------------
cardsSet2(N,R,Ls):-
    c1(N,R1,Ls),
    N1 is N-1,
    c3(N1,R2,Ls),
    c6(N1,1,1,1,R3,Ls),
    append([R1],R2,R4),
    append(R4,R3,R).

cardsSet(N,Lsalida,NC,Ls):-			
    cardsSet2(N,R,Ls),
    length(R,S),
    S=NC,
    cardsSet2(N,Lsalida,Ls),
    !.
cardsSet(N,Lsal,NC,Ls):-
    cardsSet2(N,R,Ls),
    length(R,S),
    S > NC,
    length(Lsal, NC),				% para cortar las cartas
    append(Lsal, _, R),
    !.
%--------------------------------------------------------------
tamListIgual([_|[]]):-!.		% tamaño lista igual
tamListIgual([E|Cola]):-		% suponinendo que me pongan un mazo troll xd
    nth1(1,Cola,M),
    same_length(E,M),
    tamListIgual(Cola).

eleRep([_|[]]):-!.				% elementos repetidos en una carta
eleRep([E|Cola]):-				% este se verifica cuando se aplica el anterior
    length(E,M),
    list_to_set(E,E1),
    length(E1, M1),
    M==M1,
    eleRep(Cola).

oneElemPerCard([]).				%un elemento en comun
oneElemPerCard([E|Cola]):-oneElemPerCard2([E|Cola], E), oneElemPerCard(Cola).

oneElemPerCard2([_|[]],_):-!.		
oneElemPerCard2([_|Cola],N):-		
    nth1(1,Cola,M),					
    intersection(N,M,L),			
    length(L,1),
    oneElemPerCard2(Cola,N).

cardsSetIsDobble(L):-
    tamListIgual(L),
    eleRep(L),
    oneElemPerCard(L).
%--------------------------------------------------------------    
cardsSetNthCard(Ls, N, Le):-
    nth0(N,Le, Ls).
%--------------------------------------------------------------   
cardsSetFindTotalCards(Card, S):-
    length(Card,N),
    S is ((N-1)*(N-1))+N.
%--------------------------------------------------------------
cardsSetMissingCards(Cs,MC):-	%tomar en cuenta que siempre se trabajara con una lista del tipo [a,b,c,d,e,f]
    nth1(1, Cs, R1),
    length(R1, S),
    cardsSet(S,Out,_,[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,as,at,au,av,aw,ax,ay,az,ba,bb,bc]),
    subtract(Out, Cs, MC). 
%--------------------------------------------------------------
cardsSetToString(CS,S):- cardsSetToString2(CS, S2,[],1),reverse(S2,S3),atomics_to_string(S3,"\n",S).
    
cardsSetToString2([],Str,Str,_).
cardsSetToString2([C|Cola],Str,L,I):-
    atomics_to_string(C,"-",X),
    append(["carta "],[I],Cartita),
    atomics_to_string(Cartita," ",Cartita2),
    append([Cartita2],[X],X1),
    atomics_to_string(X1,": ",X2),
    I1 is I+1,
    cardsSetToString2(Cola,Str,[X2|L],I1).
%--------------------------------------------------------------
genPlayers(1,Lp,Lp):-!.
    
genPlayers(N,Lp,X):-			  %tendra el nombre del jugador, el turno, los puntos, mazo actual.
    append([N],[0,0,[]],Player),
    N1 is N-1,
    genPlayers(N1,Lp,[Player|X]).

dobbleGame(NPlayers,CS,Mode,Game):-					%[NumPlayers,ListPlayers,Mesa,CardsSet,Modo]
    genPlayers(NPlayers,Lp,[[1,0,0,[]]]),
    reverse(CS, CS1),
    append([[]],CS1,CS2),
    reverse(CS2,CS3),
    append([NPlayers,[Lp]],[[],CS3, Mode],Game).
%--------------------------------------------------------------
dobbleGameRegister(Name,[N,[Lp],[],Cs,Mode],[N,[GameOut1],[],Cs,Mode]):-	%recibe un name, game, gameOut
    not(select([Name,0,0,[]],Lp,[Name,0,0,[]],GameOut1)),					%para cuando este el nombre repetido
    select([K,0,0,[]],Lp,[Name,0,0,[]],GameOut1),							%agregar el name
    number(K),
    !.
%--------------------------------------------------------------
showTurno([_,Turno,_,_], Turno).
showName([Name,_,_,_],Name).

turnito([_|[]],Acum,Acum).			%caso cuando la lista acaba y entrega el primer turno

turnito([X|Cola],Nombre,_):-		%caso cuando encuentra el turno
    showTurno(X,T1),
    nth1(1,Cola,N1),
    showTurno(N1,T2),
    T1>T2,
    showName(N1,Nombre).

turnito([X|Cola],Name,Acum):-		%caso que busca el turno
    showTurno(X,T1),
    nth1(1,Cola,N1),
    showTurno(N1,T2),
    T1=T2,
    turnito(Cola,Name,Acum).
    
dobbleGameWhoseTurnIsIt([_,[Pl],_,_,_],Name):-
    nth1(1,Pl, Felem),
    showName(Felem,Name1),
    turnito(Pl,Name,Name1),
    !.
%----------Ejemplo de prueba para modo de juego----------------
%stackMode(Cs,Modo):-
    
%--------------------------------------------------------------
%solo se pretende el play para el stack mode -> "stackMode".
%---null-----
dobbleGamePlay([A,[B],_,Cs,Mode],Action,[A,[B],Mesa2,Acortar,Mode]):-	
    Mode="stackMode",
    Action=null,
    length(Mesa2,2),
    append(Mesa2,Acortar,Cs),
    !.

%---pass-----
dobbleGamePlay([A,[B],Mesa,Cs,Mode],Action,[A,[B1],Mesa,Cs,Mode]):-
    Mode="stackMode",
    Action=[pass],
    dobbleGameWhoseTurnIsIt([A,[B],Mesa,Cs,Mode], Name),
    select([Name,T,P,[]],B,_,B),
    T1 is T+1,
    select([Name,_,_,[]],B,[Name,T1,P,[]],B1),
    !.

%---spotIt-----
dobbleGamePlay([A,[B],[_,[]],_,Mode],Action,Out):- 						%caso 1 queda 1 carta
    Mode="stackMode",
    Action=[spotIt,_,_],
    dobbleGamePlay([A,[B],[_,[]],_,Mode],[finish],Out),
    !.

dobbleGamePlay([A,[B],[[]],_,Mode],Action,Out):- 						%caso ya no hay mas cartas
    Mode="stackMode",
    Action=[spotIt,_,_],
    dobbleGamePlay([A,[B],[_,[]],_,Mode],[finish],Out),
    !.

dobbleGamePlay([A,[B],[E1,E2],Cs,Mode],Action,Gout):-					%caso correcto
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

dobbleGamePlay([A,[B],[E1,E2],Cs,Mode],Action,[A,[B1],[E1,E2],Cs,Mode]):- %caso incorrecto
    Mode="stackMode",
    Action=[spotIt,Nombre,Elemento],
    dobbleGameWhoseTurnIsIt([A,[B],[E1,E2],Cs,Mode], Nombre1),	
    Nombre=Nombre1,														%ve si el que juega esta de turno
    not(intersection(E1,E2,[Elemento])),
    select([Nombre,T,P,[]],B,_,B),
    T1 is T+1,															%suma 1 turno
    select([Nombre,_,_,[]],B,[Nombre,T1,P,[]],B1),
    !.
%---finish-----
dobbleGamePlay([A,[B],_,_,Mode],Action, [S2,[B],[fin],[fin],"finish"]):-
    Mode="stackMode",
    Action=[finish],
    mayorAmenor([A,[B],_,_,Mode],[],0,S1),
    puestos(S1,S2),
    !.
%----------------------------
mayorAmenor([_,[[]],_,_,_],Lmm,I,[[I]|Lmm]):-!.		%predicado que ordena a los jugaodres segun su puntaje

mayorAmenor([_,[Pl],_,_,_],Lmm,I,R):-
    select([Name,_,I,[]],Pl,_,Pl),
    append([[Name,I]],Lmm,Lmm1),
    delete(Pl,[Name,_,I,[]],Pl2),
    mayorAmenor([_,[Pl2],_,_,_],Lmm1,I,R),
    !.
           
mayorAmenor([_,[Pl],_,_,_],Lmm,I,R):-
    not(select([_,_,I,[]],Pl,_,Pl)),
    I1 is I+1,
    mayorAmenor([_,[Pl],_,_,_],Lmm,I1,R),
    !.
%----------------------------
puestos([[X]|Cola],Salida):-primerPuesto(Cola,X,[],Salida).
    
primerPuesto([First|Cola],X,R,Rf):-
    nth1(1,First,Name),
    nth1(2,First,Punto),
    X=Punto,
    append(R,[Name],R1),
    primerPuesto(Cola,X,R1,Rf),
    !.

primerPuesto([First|Cola],X,R,Rf):-
    nth1(2,First,Punto),
    not(X=Punto),
    jugadoresCola([First|Cola],[],Pierde),
    append(["perdedor/xs:"],Pierde,S1),
    append(R,S1,S2),
    append(["ganador/xs:"],S2,Rf),
    !.

primerPuesto([],_,R,Rf):-
    append(["Empate entre:"],R,Rf).

jugadoresCola([[X|_]|Cola],L,LS):-
    append([X],L,L1),
    jugadoresCola(Cola,L1,LS),
    !.
jugadoresCola([],LS,LS).
%--------------------------------------------------------------
dobbleGameStatus([_,_,_,_,Mode],"En progreso"):-
    not(Mode="finish").

dobbleGameStatus([_,_,_,_,Mode],"finalizado"):-
    Mode="finish".
%--------------------------------------------------------------
dobbleGameScore([_,[Pl],_,_,_], Name, Score):-	%score para juego 
    select([Name,_,Score,[]],Pl,_,Pl),
    !.
%--------------------------------------------------------------
jugadoresToStr([[Name,Turno,Puntos,_]|Cola],L,LS):-
    atomics_to_string(["jugador/a:",Name,"Posee:",Puntos,"Puntos","en el turno:",Turno]," ",JugadoresStr),
    append([JugadoresStr],L,L1),
    jugadoresToStr(Cola,L1,LS),
    !.
jugadoresToStr([],LS,LS).

dobbleGameToString([NumPlayers,[Pl],Mesa,Mazo,Modo],GameStr):-					%Para juego en desarrollo							
    number(NumPlayers),
    cardsSetToString(Mesa,MesaStr),
    cardsSetToString(Mazo,MazoStr),
    jugadoresToStr(Pl,[],Jugadores),
    atomics_to_string(Jugadores,"\n",JugadoresStr),
    append([],["Jugando:",Modo,"- activo\n","En la Mesa tenemos:",MesaStr,"\n",
                "En el mazo restante se tienen:" ,MazoStr,"\n","El estado de jugadores es:",
                JugadoresStr],GameStr1),
    atomics_to_string(GameStr1," ",GameStr).
                     
                   
                   
                   
                   
                   
                   

/** <examples>
?- cardsSet(3,CS,3,[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]),  dobbleGame( 4, CS, Mode, G), dobbleGameRegister("tonito", G, X), dobbleGameRegister("cri", X, X1),dobbleGameRegister("juan", X1, X2),dobbleGameRegister("daigozzz", X2, X3), dobbleGameWhoseTurnIsIt(X3, Name).
?- dobbleGameWhoseTurnIsIt([4, [[
      ["tonito", 2, 2, []],
      ["cri", 1, 1, []],
      ["juan", 1, 1, []],
      ["daigozzz", 1, 1, []]
      ]], [],[[a, b, c], [a, d, e], [a, f, g]], Mode], Name).
*/