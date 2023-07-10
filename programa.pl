% Aquí va el código.

/*------------------------------------------------------Parcial Hogwarts------------------------------------------------------*/

%============================= Parte 1-Sombrero Seleccionador =============================

% persona/4 -> Mago,Sangre,Caract,OdiaCasa
persona(harry,mestiza,[corajudo,amistoso,orgulloso,inteligente],[gryffindor,ravenclaw,hufflepuff]).
persona(draco,pura,[inteligente,orgulloso],[griffindor,ravenclaw,slytherin]).
persona(hermione,impura,[inteligente,orgulloso,responsable],[]).

% caractCasas/2-> Mago,Casa
caractCasas(Mago,gryffindor):-
    persona(Mago,_,Caract,_),
    member(coraje,Caract).

caractCasas(Mago,slytherin):-
    persona(Mago,_,Caract,_),
    member(orgullo,Caract),
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
permiteEntrar(Mago,Casa):-
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
    persona(Mago,_,_,OdiaCasa),
    permiteEntrar(Mago,Casa),
    tieneCaracterApropiado(Mago,Casa),
    not(member(Casa,OdiaACasa)).

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
acciones(fueraDeCama,-50).
acciones(irBosque,-50).
acciones(seccionRestringidaBiblioteca,-10).
acciones(tercerPiso,-75).
acciones(ganarPartidaAjedrez,50).
acciones(salvarAmigos,50).
acciones(derrotarVoldemort,60).

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
    magoHizo(Mago,Accion),
    not(malaAccion(Accion)).

% malaAccion/1 -> Accion
malaAccion(Accion):-
    acciones(Accion,Puntaje),
    Puntaje<0.

% accionRecurrente/1 -> Accion
accionRecurrente(Accion):-
    magoHizo(Mago1,Accion),
    magoHizo(Mago2,Accion),
    Mago1\=Mago2.

%Punto 2

puntajeTotalDeCasa(Casa):-
    esDe(Mago,Casa),
    magoHizo(Mago,Accion),
    findAll(Puntaje,acciones(Accion,Puntaje),Puntajes),
    sumList(Puntajes,CantPuntos).



