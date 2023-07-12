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


/*------------------------------------------------------Parcial Influencers------------------------------------------------------*/

% Punto 1
tieneCanal(ana,youtube,3000000).
tieneCanal(ana,instagram,2700000).
tieneCanal(ana,tiktok,1000000).
tieneCanal(ana,twitch,2).
tieneCanal(beto,twitch,120000).
tieneCanal(beto,youtube,6000000).
tieneCanal(beto,instagram,1100000).
tieneCanal(cami,tiktok,2000).
tieneCanal(dani,youtube,1000000).
tieneCanal(evelyn,instagram,1).

% Punto 2

% Punto 2.a
influencer(Persona):-
    tieneCanal(Persona,_,_),
    findall(Seguidores,tieneCanal(Persona,_,Seguidores),Lista),
    sumlist(Lista,SeguidoresTotales),
    SeguidoresTotales>10000.

% Punto 2.b

omnipresente(Persona):-
    tieneCanal(Persona,Red,_),
    forall(tieneCanal(_,Red,_),tieneCanal(Persona,Red,_)).

% Punto 2.c

exclusivo(Persona):-
    tieneCanal(Persona,_,_),
    findall(Red,tieneCanal(Persona,Red,_),Lista),
    length(Lista,Cantidad),
    Cantidad=<1.

% Punto 3

% Punto 3.a

%subio/4 ->Persona,Red,Archivo,caract()
subio(ana,tiktok,video,caract(1,[beto,evelyn])).
subio(ana,tiktok,video,caract(1,[ana])).
subio(ana,instagram,foto,caract([ana])).
subio(beto,instagram,foto,caract([])).
subio(cami,twitch,stream,caract(lol)).
subio(cami,youtube,video,caract(5,[cami])).
subio(evelyn,instagram,foto,caract([evelyn,cami])).

% Punto 3.b

esJuego(lol).
esJuego(minecraft).
esJuego(aoe).

% Punto 4

contenidoAdictivo(Red):-
    subio(_,Red,video,caract(Duracion,_)),
    Duracion<3.

contenidoAdictivo(Red):-
    subio(_,Red,stream,caract(Contenido)),
    esJuego(Contenido).

contenidoAdictivo(Red):-
    subio(_,Red,foto,caract(Participantes)),
    length(Participantes,Cantidad),
    Cantidad<4.

adictiva(Red):-
    subio(_,Red,_,_),
    contenidoAdictivo(Red).

% Punto 5

colaboran(Persona1,Persona2):-
    subio(Persona2,_,_,caract(_,Participantes)),
    subio(Persona1,_,_,_),
    Persona1\=Persona2,
    member(Persona1,Participantes).

colaboran(Persona1,Persona2):-
    subio(Persona2,_,_,caract(Participantes)),
    subio(Persona1,_,_,_),
    Persona1\=Persona2,
    member(Persona1,Participantes).

% Punto 6

caminoALaFama(Persona):-
    not(influencer(Persona)),
    colaboran(Persona,Persona2),
    influencer(Persona2).

caminoALaFama(Persona):-
    not(influencer(Persona)),
    colaboran(Persona,Persona2),
    caminoALaFama(Persona2).

% Punto 7

/* 7.b Simplemente no habia que escribir la relacion tieneCanal(beto,tiktok,1100000). , 
       ya que si no aparece en el enunciado que beto tiene tiktok, por universo cerrado no pertenece a nuestra base de conocimiento*/


/*------------------------------------------------------Parcial Alquimia------------------------------------------------------*/

herramienta(ana, circulo(50,3)).
herramienta(ana, cuchara(40)).
herramienta(beto, circulo(20,1)).
herramienta(beto, libro(inerte)).
herramienta(cata, libro(vida)).
herramienta(cata, circulo(100,5)).

% Punto 1

/*----------------------------------------------------Parcial de la comision de los martes-------------------------------------*/

restaurante(panchoMayo, 2, barracas).
restaurante(finoli, 3, villaCrespo).
restaurante(superFinoli, 5, villaCrespo).

menu(panchoMayo, carta(1000, pancho)).
menu(panchoMayo, carta(200, hamburguesa)).
menu(finoli, carta(2000, hamburguesa)).
menu(finoli, pasos(15, 15000, [chateauMessi, francescoliSangiovese, susanaBalboaMalbec], 6)).
menu(noTanFinoli, pasos(2, 3000, [guinoPin, juanaDama],3)).

vino( chateauMessi, francia, 5000).
vino( francescoliSangiovese, italia, 1000).
vino( susanaBalboaMalbec, argentina, 1200).
vino( christineLagardeCabernet, argentina, 5200).
vino( guinoPin, argentina, 500).
vino( juanaDama, argentina, 1000).


% Punto 1

masDeNEstrellasPorBarrio(Restaurant,EstrellasMin,Barrio):-
    restaurante(Restaurant,_,Barrio),
    forall(restaurante(Restaurant,Estrellas,_),EstrellasMin=<Estrellas).

% Punto 2

% Restaurante de prueba
restaurante(laVacaLoca,0,villaDevoto).

restauranteSinEstrellas(Restaurant):-
    restaurante(Restaurant,Estrellas,_),
    Estrellas = 0 .

% Punto 3

malOrganizado(Restaurant):-
    restaurante(Restaurant,_,_),
    menu(Restaurant,pasos(Pasos,_,ListaVinos,_)),
    length(ListaVinos,Cantidad),
    Pasos>Cantidad.

malOrganizado(Restaurant):-
    restaurante(Restaurant,_,_),
    menu(Restaurant,carta(Precio,Comida)),
    menu(Restaurant,carta(OtroPrecio,Comida)),
    OtroPrecio\=Precio.

% Punto 4

copiaBarata(Restaurant,CopiaRestaurant):-
    restaurante(Restaurant,Estrellas,_),
    restaurante(CopiaRestaurant,EstrellasCopia,_),
    menu(Restaurant,carta(Precio,Comida)),
    menu(CopiaRestaurant,carta(PrecioCopia,Comida)),
    PrecioCopia<Precio,
    EstrellasCopia<Estrellas.

% Punto 5

precioPromedio(Restaurant,Promedio):-
    findall(Precio,menu(Restaurant,carta(Precio,_)),ListaPrecios),
    sumlist(ListaPrecios,Suma),
    Promedio is (Suma/2).

precioPromedio(Restaurant,Promedio):-
    menu(Restaurant,pasos(_,Precio,ListaVinos,Comensales)),
    precioVinos(ListaVinos,PreciosVinos),
    Promedio is ((Precio+PreciosVinos)/Comensales).

precioVinos([],_).
precioVinos([Vino|Vinos],PreciosVinos):-
    vino(Vino,_,Precio),
    PreciosVinos is (PreciosVinos+Precio),
    precioVinos([Vinos],PreciosVinos).