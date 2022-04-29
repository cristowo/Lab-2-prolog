%dominios
%cuerpo : atom

%predicados
%orbita(Cuerpo,Cuerpo).
%planeta(Cuerpo).
%satelite(Cuerpo).
%cometa(Cuerpo).

%metas
%secundarias
%orbita(Cuerpo,Cuerpo).

%primarias
%planeta(Cuerpo).
%satelite(Cuerpo).
%cometa(Cuerpo).

%clausulas
%hechos
orbita(tierra,sol).
orbita(martes,sol).
orbita(mercurio,sol).
orbita(saturno,sol).
orbita(venus,sol).
orbita(jupiter,sol).
orbita(neptuno,sol).
orbita(urano,sol).
orbita(luna,tierra).
orbita(phobos,marte).
orbita(calisto,jupiter).
orbita(europa,jupiter).



%reglas
planeta(P):-orbita(P,sol).

satelite(S):-orbita(S,P),planeta(P).

cometa(C):-not(planeta(C)),not(satelite(C)).

fecha(AAAA, MMM, DDD, [DDD, MMM, AAAA]).

cartas(0, [0]).
cartas(Pa, [cartas([Pa-1] ,Pa])).

%cardsSet([a, b, c, d, e, f, g], 3, 3, CS):-
