%--------------------------------------------------------------
c1(N,M,Ls):-nth1(1,Ls,Xsimbo),N1 is N+1, c1(N,Xsimbo,M,2,N1,Ls).

c1(N,M,[M],N1,N1,Ls).
c1(N,M,[M|L],K,N1,Ls):-
    N>=K,
    nth1(K,Ls,M2),
    K1 is K+1,
    c1(N,M2,L,K1,N1,Ls).
%--------------------------------------------------------------
op(N, J, K, M):- M is (N*J)+K+1.
%--------------------------------------------------------------
c2(N,M,J,Ls):-op(N, J, 1, M2),nth1(M2,Ls,Xsimbo),N1 is N+1, c2(N,Xsimbo,M,J,2,N1,Ls).

c2(N,M,[M],J,N1,N1,Ls).
c2(N,M,[M|L],J,K,N1,Ls):-
    N>=K,
    op(N, J, K, M2),
    nth1(M2,Ls,Xsimbo),
    K1 is K+1,
    c2(N,Xsimbo, L, J, K1,N1,Ls).
%--------------------------------------------------------------
c3(N,R,Ls):-c2(N,M,1,Ls),N1 is N+1,nth1(1,Ls,Xsimbo),c3(N,[Xsimbo|M],R,2,N1,Ls).

c3(N,R,[R],N1,N1,Ls). 
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

c4(N,M,[M],I,J,N1,N1,Ls).
c4(N,M,[M|L],I,J,K,N1,Ls):-
    N>=K,
    op2(N,I,J,K,M2),
    nth1(M2,Ls,Xsimbo),
    K1 is K+1,
    c4(N,Xsimbo,L,I,J,K1,N1,Ls).
%--------------------------------------------------------------
c5(N,R,I,J,K,Ls):-c4(N,I,J,M,Ls),N1 is N+1,Iz is I+1,nth1(Iz,Ls,Xsimbo),c5(N,[Xsimbo|M],R,I,2,N1,K,Ls).

c5(N,R,[R],I,N1,N1,K,Ls). 
c5(N,R,[R|C],I,J,N1,K,Ls):-
    N>=J,
    c4(N,I,J,M,Ls),
    J1 is J+1,
    Iz is I+1,
    nth1(Iz,Ls,Xsimbo),
    c5(N,[Xsimbo|M],C,I,J1,N1,K,Ls).
%--------------------------------------------------------------
c6(N,I,J,K,R,Ls):- c5(N,M,I,J,K,Ls), N1 is N+1,c6(N,2,2,J,N1,M,R,Ls).

c6(N,N1,K,J,N1,R,R,Ls).
c6(N,I,K,J,N1,M,R,Ls):-
    N>=I,
    c5(N,M1,I,J,K,Ls),
    I1 is I+1,
    append(M,M1,Z),
    c6(N,I1,K,J,N1,Z,R,Ls).
%--------------------------------------------------------------
cardsSet(N,R,NC,Ls):-
    c1(N,R1,Ls),
    N1 is N-1,
    c3(N1,R2,Ls),
    c6(N1,1,1,1,R3,Ls),
    append([R1],R2,R4),
    append(R4,R3,R5),
    length(R, NC),				% para cortar las cartas
    append(R, _, R5).
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
oneElemPerCard2([E|Cola],N):-		
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



