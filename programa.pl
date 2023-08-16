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

jugadores(ana,agua).
jugadores(ana,vapor).
jugadores(ana,tierra).
jugadores(ana,hierro).
jugadores(beto,Elemento):-
    jugadores(ana,Elemento).
jugadores(cata,fuego).
jugadores(cata,tierra).
jugadores(cata,agua).
jugadores(cata,aire).

construccionDe(pasto,[agua,tierra]).
construccionDe(huesos,[pasto,agua]).
construccionDe(presion,[hierro,vapor]).
construccionDe(vapor,[agua,fuego]).
construccionDe(playstation,[silicio,hierro,plastico]).
construccionDe(silicio,[tierra]).
construccionDe(plastico,[huesos,presion]).

% Punto 2

tieneIngredientesPara(Persona,Elemento):-
    jugadores(Persona,_),
    construccionDe(Elemento,_),
    forall(necesita(Elemento,Ingrediente),jugadores(Persona,Ingrediente)).

necesita(Elemento,Ingrediente):-
    construccionDe(Elemento,Ingredientes),
    member(Ingrediente,Ingredientes).

% Punto 3
estaVivo(fuego).
estaVivo(agua).
estaVivo(Elemento):-
    necesita(Elemento,Ingrediente),
    estaVivo(Ingrediente).

% Punto 4

puedeConstruir(Persona,Elemento):-
    tieneIngredientesPara(Persona,Elemento),
    tieneHerramientasPara(Persona,Elemento).

tieneHerramientasPara(Persona,Elemento):-
    herramienta(Persona,Herramienta),
    sirvePara(Herramienta,Elemento).

sirvePara(libro(vida),Elemento):-
    estaVivo(Elemento).
sirvePara(libro(inerte),Elemento):-
    not(estaVivo(Elemento)).
sirvePara(Herramienta,Elemento):-
    cantidadQueSoporta(Herramienta,Soporte),
    cantidadQueNecesita(Elemento,Necesario),
    Soporte>=Necesario.

cantidadQueSoporta(cuchara(cms),Soporte):-
    Soporte is cms/10.
cantidadQueSoporta(circulo(cms,niveles),Soporte):-
    Soporte is (cms/100)*niveles.

cantidadQueNecesita(Elemento,Necesario):-
    construccionDe(Elemento,Ingredientes),
    length(Ingredientes,Necesario).

% Punto 5

primitivo(agua).
primitivo(fuego).
primitivo(tierra).
primitivo(aire).

todoPoderoso(Persona):-
    jugadores(Persona,Elemento),
    forall(jugadores(Persona,Elemento),primitivo(Elemento)).

% Punto 6

quienGana(Persona,Cantidad):-   
    jugadores(Persona,_),
    findall(Elemento,distinct(Elemento,puedeConstruir(Persona,Elemento)),Elementos),
    length(Elementos,Cantidad).


/*----------------------------------------------------Parcial Restaurant-------------------------------------------------------*/

restaurante(panchoMayo, 2, barracas).
restaurante(finoli, 3, villaCrespo).
restaurante(superFinoli, 5, villaCrespo).

menu(panchoMayo, carta(1000, pancho)).
menu(panchoMayo, carta(200, hamburguesa)).
menu(finoli, carta(2000, hamburguesa)).
menu(finoli, pasos(15, 15000, [chateauMessi, francescoliSangiovese, susanaBalboaMalbec], 6)).
menu(noTanFinoli, pasos(2, 3000, [guinoPin, juanaDama],3)).

vino(chateauMessi, francia, 5000).
vino(francescoliSangiovese, italia, 1000).
vino(susanaBalboaMalbec, argentina, 1200).
vino(christineLagardeCabernet, argentina, 5200).
vino(guinoPin, argentina, 500).
vino(juanaDama, argentina, 1000).


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

precioPromedioTotal(Restaurant,Promedio):-
    precioPromedioCarta(Restaurant,PromCarta),
    findall(PromPasos,precioPromedioPasos(Restaurant,PromPasos),PromedioPasos),
    length(PromedioPasos,CantidadPasos),
    sumlist(PromedioPasos,PromPasos2),
    Promedio is (PromCarta+PromPasos2)/(1+CantidadPasos).

precioPromedioCarta(Restaurant,Promedio):-
    findall(Precio,menu(Restaurant,carta(Precio,_)),ListaPrecios),
    sumlist(ListaPrecios,Suma),
    Promedio is (Suma/2).

precioPromedioPasos(Restaurant,Promedio):-
    menu(Restaurant,pasos(_,Precio,ListaVinos,Comensales)),
    findall(Cuanto,(member(Vino,Vinos),precioVino(Vino,Cuanto)),ListaPrecios),
    sumlist(ListaPrecios,Total),
    Promedio is ((Precio+Total)/Comensales).


precioVino(Vino,Precio):-
    vino(Vino,Pais,PrecioIndividual),
    Pais\=argentina,
    Precio is (PrecioIndividual*(1.35)).

precioVino(Vino,Precio):-
    vino(Vino,Pais,PrecioIndividual),
    Pais = argentina,
    Precio is PrecioIndividual.


/*-------------------------------------------------Parcial 31 minutos-----------------------------------------------------*/

/*
cancion(bailanSinCesar, [pabloIlabaca, rodrigoSalinas], 10600177).
cancion(yoOpino, [alvaroDiaz, carlosEspinoza, rodrigoSalinas], 5209110).
cancion(equilibrioEspiritual, [danielCastro, alvaroDiaz, pabloIlabaca, pedroPeirano, rodrigoSalinas], 12052254).
cancion(tangananicaTanganana, [danielCastro, pabloIlabaca, pedroPeirano], 5516191).
cancion(dienteBlanco, [danielCastro, pabloIlabaca, pedroPeirano], 5872927). 
cancion(lala, [pabloIlabaca, pedroPeirano], 5100530).
%Revisa el archivo del repo, que este hecho estaba con un argumento de más.
cancion(meCortaronMalElPelo, [danielCastro, alvaroDiaz, pabloIlabaca, rodrigoSalinas], 3428854).

% Mes, Puesto, Cancion
rankingTop3(febrero, 1, lala).
rankingTop3(febrero, 2, tangananicaTanganana).
rankingTop3(febrero, 3, meCortaronMalElPelo).
rankingTop3(marzo, 1, meCortaronMalElPelo).
rankingTop3(marzo, 2, tangananicaTanganana).
rankingTop3(marzo, 3, lala).
rankingTop3(abril, 1, meCortaronMalElPelo).
rankingTop3(abril, 2, dienteBlanco).
rankingTop3(abril, 3, equilibrioEspiritual).
rankingTop3(mayo, 1, meCortaronMalElPelo).
rankingTop3(mayo, 2, dienteBlanco).
rankingTop3(mayo, 3, equilibrioEspiritual).
rankingTop3(junio, 1, dienteBlanco).
rankingTop3(junio, 2, tangananicaTanganana).
rankingTop3(junio, 3, meCortaronMalElPelo).



%%%%%%% INSERTE SOLUCIÓN AQUI %%%%%%%

% ¡Éxitos! :)

/*=====================================Punto 1=====================================*/
/*Saber si una cancion es un hit, lo cual ocurre si aparece en el ranking top 3 de todos los meses*/
/*
esUnHit(Cancion):-
    cancion(Cancion,_,_),
    rankingTop3(Mes,_,_),
    forall(rankingTop3(Mes,_,_),rankingTop3(Mes,_,Cancion)).

/*=====================================Punto 2=====================================*/
/*Saber si una cancion no es reconocida por los criticos, lo cual ocurre si tiene muchas reproducciones y nunca estuvo en el ranking.
Una cancion tiene muchas reproducciones si tiene mas de 7000000 reproducciones*/
/*
noEsReconocidaPorLosCriticos(Cancion):-
    cancion(Cancion,_,Reproducciones),
    Reproducciones>7000000,
    not(rankingTop3(_,_,Cancion)).

/*===================================== Punto 3 =====================================*/
/*Saber si dos compositores son colaboradores, lo cual ocurre si compusieron alguna cancion juntos*/
/*
sonColaboradores(Artista,Compositor):-
    cancion(Cancion,Artistas,_),
    member(Artista,Artistas),
    member(Compositor,Artistas),
    Artista\=Compositor.

/*===================================== Punto 4 =====================================*/
/*Modelar en la solucion a los siguientes trabajadores:
    Tulio, conductor con 5 años de experiencia
    Bodoque, periodista con 5 años de experiencia con un titulo de licenciatura, y tambiuen reportero con 5 años de experiencia y 300 notas realizadas
    Mario Hugo, periodista con 10 añños de experencia con un posgrado
    Juanin, conductor que recien empieza asi que no tiene años de experiencia*/
/*
trabajador(tulio,trabajo(conductor,5)).
trabajador(bodoque,trabajo(periodista,5,licenciatura)).
trabajador(bodoque,trabajo(reportero,5,300)).
trabajador(marioHugo,trabajo(periodista,10,posgrado)).
trabajador(juanin,trabajo(conductor,0)).

/*=================================== Punto 5 =====================================*/
/*  Conocer el sueldo total de una persona, el cual esta dado ppor la suma de los sueldos de cada uno de sus trabajos. El sueldo de cada trabajo se calcula de la siguiente forma:
    El sueldo de un conductor es de 10000 por cadad año de experiencia
    El sueldo de un reportero tambien es 10000 por cada año de experiencia mas 100 por cada nota que haya hecho en su carrera
    El sueldo de los periodistas, por cada año de experiecia reciben 5000, pero se les aplica un porcentaje de incremento del 20% cuando tienen una licenciatura
        o del 35% si tiene un posgrado
*/ 

/*
sueldosTrabajos(conductor,10000).
sueldosTrabajos(reportero,10000,100).
sueldosTrabajos(periodista,5000).

aumentoSegunTitulo(posgrado,1.35).
aumentoSegunTitulo(licenciatura,1.20).

sueldoTotal(Persona,SueldoTotal):-
    trabajador(Persona,_),
    findall(Sueldo,sueldoSegunTrabajo(Persona,Sueldo),Sueldos),
    sum_list(Sueldos, SueldoTotal).

sueldoSegunTrabajo(Persona,Sueldo):-
    trabajador(Persona,trabajo(conductor,Anios)),
    sueldosTrabajos(conductor,MontoAnios),
    Sueldo is (Anios*MontoAnios).

sueldoSegunTrabajo(Persona,Sueldo):-
    trabajador(Persona,trabajo(reportero,Anios,Notas)),
    sueldosTrabajos(reportero,MontoAnios,MontoNotas),
    Sueldo is ((Anios*MontoAnios)+(Notas*MontoNotas)).

sueldoSegunTrabajo(Persona,Sueldo):-
    trabajador(Persona,trabajo(periodista,Anios,Titulo)),
    sueldosTrabajos(periodista,MontoAnios),
    aumentoSegunTitulo(Titulo,MontoRecargo),
    Sueldo is (MontoAnios*Anios)*MontoRecargo.

/*=================================== Punto 6 =====================================*/
/*Agregar un nuevo trabajador que tenga otro tipo de trabajo nuevo, distinto a los anteriores.
Agregar una forma de calcular el sueldo para el nuevo trabajo agregado
¿Que concepto de la materia se puede relacionar a esto?*/
/*
trabajador(juanito,trabajo(limpieza,10,noche,5)).

/*Juanito es un encargado de la limpieza con 10 años de experiencia y que trabaja en el turno noche y limpia 5 baños en su turno*/
/*
sueldosTrabajos(limpieza,5000).

dificultadTurno(manana,2).
dificultadTurno(tarde,3).
dificultadTurno(noche,5).

/*El sueldo consiste en 5000 por, año de experiencia mas la cantidad de baños que limpia por la dificultad del turno*/
    /*
sueldoSegunTrabajo(Persona,Sueldo):-
    trabajador(Persona,trabajo(limpieza,Anios,Turno,Banios)),
    sueldosTrabajos(limpieza,MontoAnios),
    dificultadTurno(Turno,Dificultad),
    Sueldo is MontoAnios*(Anios+(Banios*Dificultad)).

/*El concepto visto en clase que hace que podamos lograr lo anterior, sin tener que modificar el codigo del punto 5, es el de polimorfismo*/

*/

/*-----------------------------------------------Parcial Rally Dakar------------------------------------------------------------*/

/*
auto(modelo)
moto(anioDeFabricacion, suspensionesExtras)
camion(items)
cuatri(marca)
*/

% ganador/3 -> Anio Corredor Vehiculo
ganador(1997,peterhansel,moto(1995, 1)).
ganador(1998,peterhansel,moto(1998, 1)).
ganador(2010,sainz,auto(touareg)).
ganador(2010,depress,moto(2009, 2)).
ganador(2010,karibov,camion([vodka, mate])).
ganador(2010,patronelli,cuatri(yamaha)).
ganador(2011,principeCatar,auto(touareg)).
ganador(2011,coma,moto(2011, 2)).
ganador(2011,chagin,camion([repuestos, mate])).
ganador(2011,patronelli,cuatri(yamaha)).
ganador(2012,peterhansel,auto(countryman)).
ganador(2012,depress,moto(2011, 2)).
ganador(2012,deRooy,camion([vodka, bebidas])).
ganador(2012,patronelli,cuatri(yamaha)).
ganador(2013,peterhansel,auto(countryman)).
ganador(2013,depress,moto(2011, 2)).
ganador(2013,nikolaev,camion([vodka, bebidas])).
ganador(2013,patronelli,cuatri(yamaha)).
ganador(2014,coma,auto(countryman)).
ganador(2014,coma,moto(2013, 3)).
ganador(2014,karibov,camion([tanqueExtra])).
ganador(2014,casale,cuatri(yamaha)).
ganador(2015,principeCatar,auto(countryman)).
ganador(2015,coma,moto(2013, 2)).
ganador(2015,mardeev,camion([])).
ganador(2015,sonic,cuatri(yamaha)).
ganador(2016,peterhansel,auto(2008)).
ganador(2016,prince,moto(2016, 2)).
ganador(2016,deRooy,camion([vodka, mascota])).
ganador(2016,patronelli,cuatri(yamaha)).
ganador(2017,peterhansel,auto(3008)).
ganador(2017,sunderland,moto(2016, 4)).
ganador(2017,nikolaev,camion([ruedaExtra])).
ganador(2017,karyakin,cuatri(yamaha)).
ganador(2018,sainz,auto(3008)).
ganador(2018,walkner,moto(2018, 3)).
ganador(2018,nicolaev,camion([vodka, cama])).
ganador(2018,casale,cuatri(yamaha)).
ganador(2019,principeCatar,auto(hilux)).
ganador(2019,prince,moto(2018, 2)).
ganador(2019,nikolaev,camion([cama, mascota])).
ganador(2019,cavigliasso,cuatri(yamaha)).


% pais/2 -> Corredor Pais
pais(peterhansel,francia).
pais(sainz,espania).
pais(depress,francia).
pais(karibov,rusia).
pais(patronelli,argentina).
pais(principeCatar,catar).
pais(coma,espania).
pais(chagin,rusia).
pais(deRooy,holanda).
pais(nikolaev,rusia).
pais(casale,chile).
pais(mardeev,rusia).
pais(sonic,polonia).
pais(prince,australia).
pais(sunderland,reinoUnido).
pais(karyakin,rusia).
pais(walkner,austria).
pais(cavigliasso,argentina).


% Punto 1
% marcas/2 -> Marca, Modelo
marcas(peugeot,2008).
marcas(peugeot,3008).
marcas(mini,countryman).
marcas(volkswagen,touareg).
marcas(toyota,hilux).

/*para agregar el modelo buggy en mini, solamente debemos de colocar "marcas(mini,buggy)", y para el dkr simplemente no agregamos nada, ya que por universo cerrado no pertenece a nuestra
base de reconocimiento*/

% Punto 2

ganadorReincidente(Corredor):-
    ganador(Anio,Corredor,_),
    ganador(Ani2,Corredor,_),
    Anio\=Anio2.

% Punto 3

mismoPais(Corredor1,Corredor2):-
    pais(Corredor1,Pais),
    pais(Corredor2,Pais).

inspiraA(Inspirador,Inspirado):-
    not(ganador(_,Inspirado,_)),
    ganador(_,Inspirador,_),
    mismoPais(Inspirado,Inspirador).

inspiraA(Inspirador,Inspirado):-
    ganador(AnioInspirado,Inspirado,_),
    ganador(AnioInpirador,Inspirador,_),
    AnioInspirado>AnioInpirador,
    mismoPais(Inspirado,Inspirador).

% Punto 4

marcaDeLaFortuna(Conductor,Marca):-
    ganador(_,Conductor,Vehiculo),
    obtenerMarca(Vehiculo,Marca),
    not(ganoConOtraMarca(Conductor,Marca,Vehiculo)).

ganoConOtraMarca(Conductor,Marca,Vehiculo):-
    obtenerMarca(Vehiculo,Marca),
    obtenerMarca(Vehiculo,OtraMarca),
    Marca\=OtraMarca.


obtenerMarca(auto(Modelo),Marca):-
    marcas(Marca,Modelo).

obtenerMarca(cuatri(Marca),Marca).

obtenerMarca(moto(Anio,_),ktm):-
    antesDe(2000,moto(Anio,_)).

obtenerMarca(moto(Anio,_),yamaha):-
    not(antesDe(2000,moto(Anio,_))).

antesDe(Anio,moto(AnioFabri,_)):-
    Anio>=AniosFabri.

obtenerMarca(camion(Elementos),kamaz):-
    tiene(vodka,Elementos).

obtenerMarca(camion(Elementos),iveco):-
    not(tiene(vodka,Elementos)).

tiene(Elemento,Elementos):-
    member(Elemento,Elementos).

/*--------------------------------------------------------------------------Parcial re Hecho-----------------------------------------------------------*/

% Punto 1

% Cancion, Compositores,  Reproducciones
cancion(bailanSinCesar, [pabloIlabaca, rodrigoSalinas], 10600177).
cancion(yoOpino, [alvaroDiaz, carlosEspinoza, rodrigoSalinas], 5209110).
cancion(equilibrioEspiritual, [danielCastro, alvaroDiaz, pabloIlabaca, pedroPeirano, rodrigoSalinas], 12052254).
cancion(tangananicaTanganana, [danielCastro, pabloIlabaca, pedroPeirano], 5516191).
cancion(dienteBlanco, [danielCastro, pabloIlabaca, pedroPeirano], 5872927). 

cancion(lala, [pabloIlabaca, pedroPeirano], 5100530).
%Revisa el archivo del repo, que este hecho estaba con un argumento de más.

cancion(meCortaronMalElPelo, [danielCastro, alvaroDiaz, pabloIlabaca, rodrigoSalinas], 3428854).

% Mes, Puesto, Cancion
rankingTop3(febrero, 1, lala).
rankingTop3(febrero, 2, tangananicaTanganana).
rankingTop3(febrero, 3, meCortaronMalElPelo).
rankingTop3(marzo, 1, meCortaronMalElPelo).
rankingTop3(marzo, 2, tangananicaTanganana).
rankingTop3(marzo, 3, lala).
rankingTop3(abril, 1, tangananicaTanganana).
rankingTop3(abril, 2, dienteBlanco).
rankingTop3(abril, 3, equilibrioEspiritual).
rankingTop3(mayo, 1, meCortaronMalElPelo).
rankingTop3(mayo, 2, dienteBlanco).
rankingTop3(mayo, 3, equilibrioEspiritual).
rankingTop3(junio, 1, dienteBlanco).
rankingTop3(junio, 2, tangananicaTanganana).
rankingTop3(junio, 3, lala).

% Punto 1

esUnHit(Cancion):-
    cancion(Cancion,_,_),
    forAll(rankingTop3(Mes,_,_),rankingTop3(Mes,_,Cancion)).

% Punto 2

noEsReconocidaPorLosCriticos(Cancion):-
    cancion(Cancion,_,Reproducciones),
    not(rankingTop3(_,_,Cancion)),
    Reproducciones>7000000.

% Punto 3

sonColaboradores(Artista,Colaborador):-
    cancion(_,Compositores,_),
    member(Artista,Compositores),
    member(Colaborador,Compositores),
    Artista\=Colaborador.

% Punto 4

trabajadores(tulio,trabajo(conductor,5)).
trabajadores(bodoque,trabajo(periodista,2,licenciatura)).
trabajadores(bodoque,trabajo(reportero,5,300)).
trabajadores(marioHugo,trabajo(periodista,10,posgrado)).
trabajadores(juanin,trabajo(conductor,0)).

% Punto 5

sueldoTrabajo(conductor,10000).
sueldoTrabajo(periodista,5000).
sueldoTrabajo(reportero,10000,100).

incrementoPorEstudio(licenciatura,1.20).

incrementoPorEstudio(posgrado,1.35).

sueldoTotal(Trabajador,SueldoTotal):-
    trabajadores(Trabajador,_),
    findall(Sueldo,sueldosPorTrabajo(Trabajador,Sueldo),Sueldos).
    sum_list(Sueldos,SueldoTotal).

sueldosPorTrabajo(Trabajador,Sueldo):-
    trabajadores(Trabajador,trabajo(conductor,Anios)),
    sueldoTrabajo(conductor,MontoAnios),
    Sueldo is Anios*MontoAnios.

sueldosPorTrabajo(Trabajador,Sueldo):-
    trabajadores(Trabajador,trabajo(reportero,Anios,Notas)),
    sueldoTrabajo(reportero,MontoAnios,MontoNotas),
    Sueldo is (MontoAnios*Anios)+(MontoNotas*Notas).

sueldosPorTrabajo(Trabajador,Sueldo):-
    trabajadores(Trabajador,trabajo(periodista,Anios,Titulo)),
    sueldoTrabajo(periodista,MontoAnios),
    calcularIncremento(Titulo,Incremento)
    Sueldo is (MontoAnios*Anios)*Incremento.

% Punto 6

trabajadores(juanito,trabajo(camarografo,10,principal,50)).

/*Juanito es un camarografo con 10 años de experiencia, utiliza la camara principal y realiza 50 zooms dramaticos por dia*/

sueldoTrabajo(camarografo,5000,10).

extraPorCamara(principal,5000).
%Se pueden agregar mas tipos de camaras

/*Cobra 5000 por cada año de experiencia, se le descuentan 10 de cada zoom dramatico y dependiendo el tipo de camara que maneje, se le suma un extra*/


sueldosPorTrabajo(Trabajador,Sueldo):-
    trabajadores(Trabajador,trabajo(camarografo,Anios,Camara,Zooms)),
    extraPorCamara(Camara,Extra),
    sueldoTrabajo(camarografo,MontoAnios,MontoZoom),
    Sueldo is (MontoAnios*Anios)+(Extra)-(MontoZoom*Zooms).


/*Se utiiliza el concepto de polimorfismo*/