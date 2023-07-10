    % Aquí va el código.

/*------------------------------------------------------Parcial Hogwarts------------------------------------------------------*/

%============================= Parte 1-Sombrero Seleccionador =============================

% persona/4 -> Mago,Sangre,Caract,noOdiaCasa
persona(harry,mestiza,[corajudo,amistoso,orgulloso,inteligente],[gryffindor,ravenclaw,hufflepuff]).
persona(draco,pura,[inteligente,orgulloso],[griffindor,ravenclaw,slytherin]).
persona(hermione,impura,[inteligente,orgulloso,responsable],[]).

% caractCasas/2-> Mago,Casa
caractCasas(Mago,gryffindor):-
    persona(Mago,_,Caract,_),
    member(corajudo,Caract).

caractCasas(Mago,slytherin):-
    persona(Mago,_,Caract,_),
    member(orgulloso,Caract),
    member(inteligente,Caract).

caractCasas(Mago,ravenclaw):-
    persona(Mago,_,Caract,_),
    member(inteligente,Caract),
    member(responsable,Caract).

caractCasas(Mago,hufflepuff):-
    persona(Mago,_,Caract,_),
    member(amistoso,Caract).

% Punto 1
% permiteEntrar/2 -> Mago, Casa
permiteEntrar(Mago,ravenclaw):-
    persona(Mago,_,_,_),
    caractCasas(Mago,Casa).

permiteEntrar(Mago,gryffindor):-
    persona(Mago,_,_,_),
    caractCasas(Mago,Casa).

permiteEntrar(Mago,hufflepuff):-
    persona(Mago,_,_,_),
    caractCasas(Mago,Casa).

permiteEntrar(Mago,slytherin):-
    persona(Mago,Sangre,_,_),
    Sangre\=impura,
    caractCasas(Persona,slytherin).
% Punto 2
% tieneCaracterApropiado/2 -> Mago,Casa
tieneCaracterApropiado(Mago,Casa):-
    persona(Mago,_,_,_),
    caractCasas(Mago,Casa).
% Punto 3
% puedeQuedarSeleccionado/2 -> Mago,Casa
puedeQuedarSeleccionado(Mago,Casa):-
    persona(Mago,_,_,NoOdiaCasa),
    permiteEntrar(Mago,Casa),
    tieneCaracterApropiado(Mago,Casa),
    member(Casa,NoOdiaCasa).

puedeQuedarSeleccionado(hermione,gryffindor).

% Punto 4
% cadenaDeAmistades/1 -> ListaMagos
cadenaDeAmistades([X|Xs]):-
    persona(X,_,Caract,_),
    member(amistoso,Caract),
    puedenEnMismaCasa(X,cadenaDeAmistades(Xs)).

puedenEnMismaCasa(Mago1,Mago2):-
    persona(Mago1,_,_,_),
    persona(Mago2,_,_,_),
    puedeQuedarSeleccionado(Mago1,Casa1),
    puedeQuedarSeleccionado(Mago2,Casa2),
    Casa1 = Casa2.

%============================= Parte 2-La Copa de las Casas =============================

% acciones/2 -> Accion,Puntaje
accion(fueraDeCama,(-50)).
accion(irBosque,(-50)).
accion(seccionRestringidaBiblioteca,(-10)).
accion(tercerPiso,(-75)).
accion(ganarPartidaAjedrez,50).
accion(salvarAmigos,50).
accion(derrotarVoldemort,60).
accion(irMazmorras,15).

% magoHizo/2 -> Mago, Accion
magoHizo(harry,fueraDeCama).
magoHizo(hermione,tercerPiso).
magoHizo(hermione,seccionRestringidaBiblioteca).
magoHizo(harry,irBosque).
magoHizo(harry,tercerPiso).
magoHizo(draco,irMazmorras).
magoHizo(ron,ganarPartidaAjedrez).
magoHizo(hermione,salvarAmigos).
magoHizo(harry,derrotarVoldemort).

% esDe/2 -> Mago,Casa
esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).


% Punto 1

% buenMago/1 -> Mago
buenMago(Mago):-
    forall(magoHizo(Mago,Accion),not(malaAccion(Accion))).

% malaAccion/1 -> Accion
malaAccion(Accion):-
    accion(Accion,Puntaje),
    Puntaje<0.

% accionRecurrente/1 -> Accion
accionRecurrente(Accion):-
    magoHizo(Mago1,Accion),
    magoHizo(Mago2,Accion),
    Mago1\=Mago2.

%Punto 2

% puntos/3 -> Mago,Accion,Puntos
puntos(Mago,Accion,Puntos):-
    magoHizo(Mago,Accion),
    accion(Accion,P),
    Puntos is P.

% puntajeMago/2 -> Casa,Puntos
puntajeDeCasaPorMago(Casa,PuntosTotales):-
    esDe(Mago,Casa),
    findall(Puntos,puntos(Mago,Accion,Puntos),Lista),
    sumlist(Lista,PuntosTotales).

% puntajeTotalDeCasa/2 -> Casa,PuntosTotales
puntajeTotalDeCasa(Casa,Puntos):-
    puntajeDeCasaPorMago(Casa,PuntosTotales),
    findall(Puntos,puntajeDeCasaPorMago(Casa,Puntos),Lista),
    sumlist(Lista,Puntos).


% Punto 3
% casaGanadora/1 -> Casa
casaGanadora(Ganador):-
    esDe(_,Ganador),
    puntajeTotalDeCasa(Ganador,Puntos1),
    forall(puntajeTotalDeCasa(_,Puntos2),Puntos1>=Puntos2).

% Punto 4

magoHizo(hermione,pregunta(encuentraBezoar,20,snape)).
magoHizo(hermione,pregunta(levitarPluma,25,flitwick)).

puntos(Mago,Puntos):-
    magoHizo(Mago,pregunta(_,P,Profesor)),
    Profesor = snape,
    Puntos is P/2.

puntos(Mago,Puntos):-
    magoHizo(Mago,pregunta(_,P,Profesor)),
    Profesor \= snape,
    Puntos is P.

puntajeDeCasaPorMago(Casa,PuntosTotales):-
    esDe(Mago,Casa),
    findall(Puntos,puntos(Mago,Puntos),Lista),
    sumlist(Lista,PuntosTotales).