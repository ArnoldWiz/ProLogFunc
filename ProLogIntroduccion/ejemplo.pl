plato(principal, carne).
plato(postre, helado).

consultar_plato(Tipo, Plato) :- plato(Tipo, Plato).

menu(dia, sopa, plato_principal, postre).
menu(vegetariano, ensalada, tofu, fruta).

es_menu(X, Y, Z, W) :- menu(X, Y, Z, W).

entrada(antipasto).
entrada(sopa). 
entrada(queso). 

carne(milanesa).
carne(chorizo).
carne(pollo_asado).

pescado(congrio).
pescado(pejerey).

postre(flan).
postre(helado).
postre(fruta).

plato_principal(P) :- carne(P).
plato_principal(P) :- pescado(P).
comida(E,P,D) :- entrada(E), plato_principal(P), postre(D).

calorias(antipasto, 300).
calorias(sopa, 200).
calorias(queso, 400).
calorias(milanesa, 500).
calorias(chorizo, 600).
calorias(pollo_asado, 450).
calorias(congrio, 350).
calorias(pejerey, 400).
calorias(flan, 300).
calorias(helado, 250).
calorias(fruta, 100).

valor(E,P,D,V) :- calorias(E,X), calorias(P,Y), calorias(D,Z), V is X+Y+Z.