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
    append([NPlayers,[Lp]],[[],CS, Mode],Game).
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
dobbleGamePlay([A,[B],_,Cs,C],Action,[A,[B],Mesa2,Acortar,C]):-
    Action=null,
    length(Mesa2,2),
    append(Mesa2,Acortar,Cs),
    !.
dobbleGamePlay([A,[B],Mesa,Cs,C],Action,[A,[B1],Mesa,Cs,C]):-
    Action=[pass],
    dobbleGameWhoseTurnIsIt([A,[B],Mesa,Cs,C], Name),
    select([Name,T,P,[]],B,_,B),
    T1 is T+1,
    select([Name,_,_,[]],B,[Name,T1,P,[]],B1),
    !.

dobbleGamePlay([A,[B],[E1,E2],Cs,C],Action,Gout):-	%caso correcto
    Action=[spotIt,Nombre,Elemento],
    intersection(E1,E2,[Elemento]),										%ve respuesta buena o mala
    select([Nombre,T,P,[]],B,_,B),										%encuentra el nombre
    T1 is T+1,															%suma 1 turno
    P1 is P+1,															%suma 1 punto
    select([Nombre,_,_,[]],B,[Nombre,T1,P1,[]],B1),
    dobbleGamePlay([A,[B1],[E1,E2],Cs,C],null,Gout),					%siguientes 2 cartas
    !.

dobbleGamePlay([A,[B],[E1,E2],Cs,C],Action,[A,[B1],[E1,E2],Cs,C]):-	%caso incorrecto
    Action=[spotIt,Nombre,Elemento],
    not(intersection(E1,E2,[Elemento])),
    select([Nombre,T,P,[]],B,_,B),
    T1 is T+1,															%suma 1 turno
    select([Nombre,_,_,[]],B,[Nombre,T1,P,[]],B1),
    !.






/** <examples>
?- cardsSet(3,CS,3,[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]),  dobbleGame( 4, CS, Mode, G), dobbleGameRegister("tonito", G, X), dobbleGameRegister("cri", X, X1),dobbleGameRegister("juan", X1, X2),dobbleGameRegister("daigozzz", X2, X3), dobbleGameWhoseTurnIsIt(X3, Name).
?- dobbleGameWhoseTurnIsIt([4, [[
      ["tonito", 2, 2, []],
      ["cri", 1, 1, []],
      ["juan", 1, 1, []],
      ["daigozzz", 1, 1, []]
      ]], [],[[a, b, c], [a, d, e], [a, f, g]], Mode], Name).
*/