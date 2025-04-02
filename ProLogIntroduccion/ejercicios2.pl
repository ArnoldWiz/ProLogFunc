% Planificador de gastos
% Definir los hechos
costo_alojamiento(hotel, 100).
costo_alojamiento(hostal, 50).
costo_alojamiento(camping, 20).

costo_transporte(avion, 300).
costo_transporte(tren, 100).
costo_transporte(bus, 50).

costo_comida(economica, 10).
costo_comida(estandar, 30).
costo_comida(lujo, 60).

costo_viaje(Lugar, TipoAlojamiento, NumDias, TipoTransporte, TipoComida, CostoTotal) :-
    costo_alojamiento(TipoAlojamiento, CostoAlojamiento),
    costo_transporte(TipoTransporte, CostoTransporte),
    costo_comida(TipoComida, CostoComida),
    CostoTotal is (CostoAlojamiento * NumDias) + CostoTransporte + (CostoComida * NumDias).

%horoscopo
dias_mes(1, 31).  % Enero
dias_mes(2, 28).  % Febrero (no considera años bisiestos)
dias_mes(3, 31).  % Marzo
dias_mes(4, 30).  % Abril
dias_mes(5, 31).  % Mayo
dias_mes(6, 30).  % Junio
dias_mes(7, 31).  % Julio
dias_mes(8, 31).  % Agosto
dias_mes(9, 30).  % Septiembre
dias_mes(10, 31). % Octubre
dias_mes(11, 30). % Noviembre
dias_mes(12, 31). % Diciembre

horoscopo(aries, 21, 3, 21, 4).
horoscopo(tauro, 21, 4, 21, 5).
horoscopo(geminis, 21, 5, 21, 6).
horoscopo(cancer, 21, 6, 21, 7).
horoscopo(leo, 21, 7, 21, 8).
horoscopo(virgo, 21, 8, 21, 9).
horoscopo(libra, 21, 9, 21, 10).
horoscopo(escorpio, 21, 10, 21, 11).
horoscopo(sagitario, 21, 11, 21, 12).
horoscopo(capricornio, 21, 12, 21, 1).
horoscopo(acuario, 21, 1, 21, 2).
horoscopo(piscis, 21, 2, 21, 3).

dia_valido(Dia, Mes) :-
    dias_mes(Mes, MaxDia), Dia >= 1, Dia =< MaxDia.
    
signo(Dia, Mes, Signo) :- 
    dia_valido(Dia, Mes),  % Verifica si el día es válido
    horoscopo(Signo, D1, M1, D2, M2),
    ((Mes = M1, Dia >= D1) ; (Mes = M2, Dia =< D2)).

%enfermedades
enfermo_de(manuel, gripe).  
tiene_sintoma(alicia, cansancio).  

sintoma_de(fiebre, gripe).  
sintoma_de(tos, gripe).  
sintoma_de(cansancio, anemia).  

elimina(vitaminas, cansancio).  
elimina(aspirinas, fiebre).  
elimina(jarabe, tos).  

recetar_a(X, Y) :- enfermo_de(Y, A), alivia(X, A).  
alivia(X, Y) :- elimina(X, A), sintoma_de(A, Y).  

enfermo_de(X, Y) :- tiene_sintoma(X, Z), sintoma_de(Z, Y).

