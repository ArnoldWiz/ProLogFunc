:- module(plantas_medicinales, [
    interfaz/0,
    planta/1,
    nombre_cientifico/2,
    origen/3,
    modo_preparacion/2,
    enfermedad/1,
    trata/2,
    accion/2,
    precaucion/2,
    sintoma/2
]).

:- use_module(library(pce)).

:- dynamic planta/1. 
:- dynamic nombre_cientifico/2. 
:- dynamic origen/3. 
:- dynamic modo_preparacion/2. 
:- dynamic enfermedad/1. 
:- dynamic trata/2. 
:- dynamic accion/2. 
:- dynamic precaucion/2. 
:- dynamic sintoma/2.

% Declaración para evitar warnings
:- discontiguous precaucion/2.
:- discontiguous accion/2.
:- discontiguous trata/2.
:- discontiguous modo_preparacion/2.
:- discontiguous origen/3.
:- discontiguous nombre_cientifico/2.

% ---------------------------------- % HECHOS % ----------------------------------

% ABROJO (Pag. 36) 
planta(abrojo). 
nombre_cientifico(abrojo, 'Tribulus cistoides'). 
origen(abrojo, america, mexico). 
modo_preparacion(abrojo, cocimiento). 
enfermedad(infeccion_urinaria). 
enfermedad(cistitis). 
trata(abrojo, infeccion_urinaria). 
trata(abrojo, cistitis). 
accion(abrojo, diuretico). 
accion(abrojo, antiinflamatorio). 
precaucion(abrojo, embarazo).

% ACONITO (Pag. 41) 
planta(aconito). 
nombre_cientifico(aconito, 'Aconitum napellus'). 
origen(aconito, europa, desconocido). 
modo_preparacion(aconito, tintura). 
enfermedad(neuralgia). 
enfermedad(fiebre). 
enfermedad(reumatismo). 
trata(aconito, neuralgia). 
trata(aconito, fiebre). 
trata(aconito, reumatismo). 
accion(aconito, analgesico). 
accion(aconito, antipiretico). 
precaucion(aconito, alta_toxicidad).

% ADORMIDERA (Pag. 42)
planta(adormidera).
nombre_cientifico(adormidera, 'Papaver somniferum').
origen(adormidera, asia, desconocido).
modo_preparacion(adormidera, infusion).
modo_preparacion(adormidera, extracto).
enfermedad(insomnio).
enfermedad(dolor).
enfermedad(ansiedad).
trata(adormidera, insomnio).
trata(adormidera, dolor).
trata(adormidera, ansiedad).
accion(adormidera, sedante).
precaucion(adormidera, dependencia).

% AGUACATE (Pag. 43)
planta(aguacate).
nombre_cientifico(aguacate, 'Persea americana').
origen(aguacate, america, mexico).
modo_preparacion(aguacate, infusion).
modo_preparacion(aguacate, cocimiento).
enfermedad(estrenimiento).
enfermedad(problemas_digestivos).
enfermedad(tos).
trata(aguacate, estrenimiento).
trata(aguacate, problemas_digestivos).
trata(aguacate, tos).
accion(aguacate, laxante).
accion(aguacate, digestivo).

% AHUEHUETE (Pag. 44)
planta(ahuehuete).
nombre_cientifico(ahuehuete, 'Taxodium mucronatum').
origen(ahuehuete, america, mexico).
modo_preparacion(ahuehuete, decoccion).
enfermedad(enfermedades_respiratorias).
enfermedad(infecciones_piel).
trata(ahuehuete, enfermedades_respiratorias).
trata(ahuehuete, infecciones_piel).
accion(ahuehuete, antiseptico).
accion(ahuehuete, expectorante).

% AJENJO (Pag. 45)
planta(ajenjo).
nombre_cientifico(ajenjo, 'Artemisia absinthium').
origen(ajenjo, europa, desconocido).
modo_preparacion(ajenjo, infusion).
modo_preparacion(ajenjo, macerado).
enfermedad(parasitos_intestinales).
enfermedad(problemas_digestivos).
enfermedad(debilidad_general).
trata(ajenjo, parasitos_intestinales).
trata(ajenjo, problemas_digestivos).
trata(ajenjo, debilidad_general).
accion(ajenjo, vermifugo).
accion(ajenjo, tonico).
precaucion(ajenjo, embarazo).

% AJO (Pag. 46)
planta(ajo).
nombre_cientifico(ajo, 'Allium sativum').
origen(ajo, asia, 'siberia,norte_de_iran').
modo_preparacion(ajo, con_miel).
modo_preparacion(ajo, machacar).
modo_preparacion(ajo, molido_con_leche).
enfermedad(reumas).
enfermedad(sarna).
enfermedad(tina).
enfermedad(callos).
enfermedad(lombrices).
trata(ajo, reumas).
trata(ajo, sarna).
trata(ajo, tina).
trata(ajo, callos).
trata(ajo, lombrices).
accion(ajo, antiinflamatorio).
accion(ajo, vermifugo).
accion(ajo, febrifugo).
accion(ajo, diuretico).
accion(ajo, expectorante).
accion(ajo, antiparasitario).
precaucion(ajo, problemas_gastrointestinales).

% ALBAHACA (Pag. 47)
planta(albahaca).
nombre_cientifico(albahaca, 'Ocimum basilicum').
origen(albahaca, asia, india).
modo_preparacion(albahaca, jugo).
enfermedad(alopecia).
trata(albahaca, alopecia).
accion(albahaca, tonico_capilar).
accion(albahaca, diuretico).
accion(albahaca, carminativo).
accion(albahaca, emenagogo).
precaucion(albahaca, reacciones_alergicas).

% ALCACHOFA (Pag. 48)
planta(alcachofa).
nombre_cientifico(alcachofa, 'Cynara scolymus').
origen(alcachofa, africa, egipto).
modo_preparacion(alcachofa, cocer).
enfermedad(diabetes).
enfermedad(anemia).
trata(alcachofa, diabetes).
trata(alcachofa, anemia).
accion(alcachofa, descongestionante).
accion(alcachofa, desinflamatorio).
precaucion(alcachofa, alergias).

% ALCANFOR (Pag. 49)
planta(alcanfor).
nombre_cientifico(alcanfor, 'Laurus camphora').
origen(alcanfor, asia, taiwan).
modo_preparacion(alcanfor, aceite).
enfermedad(gota).
enfermedad(piquetes_mosco).
enfermedad(tifoidea).
enfermedad(artritis).
enfermedad(arteriosclerosis).
trata(alcanfor, gota).
trata(alcanfor, piquetes_mosco).
trata(alcanfor, tifoidea).
trata(alcanfor, artritis).
trata(alcanfor, arteriosclerosis).
accion(alcanfor, analgesico).
accion(alcanfor, antiseptico).
precaucion(alcanfor, no_usar_en_exceso).

% AMAPOLA AMARILLA (Pag. 50)
planta(amapola_amarilla).
nombre_cientifico(amapola_amarilla, 'Eschscholtzia californiana').
origen(amapola_amarilla, america, mexico).
modo_preparacion(amapola_amarilla, infusion).
enfermedad(diarrea).
enfermedad(insomnio).
enfermedad(ansiedad_leve).
trata(amapola_amarilla, diarrea).
trata(amapola_amarilla, insomnio).
trata(amapola_amarilla, ansiedad_leve).
accion(amapola_amarilla, sedante_suave).
accion(amapola_amarilla, antiespasmodico).
precaucion(amapola_amarilla, evitar_en_embarazo).
precaucion(amapola_amarilla, evitar_en_lactancia).

% AMATE (Pag. 51)
planta(amate).
nombre_cientifico(amate, 'Ficus insipida').
origen(amate, america, mexico).
modo_preparacion(amate, horchata).
enfermedad(reumatismo).
enfermedad(diviesos).
enfermedad(solitaria).
enfermedad(inflamacion).
enfermedad(infecciones_leves).
trata(amate, reumatismo).
trata(amate, diviesos).
trata(amate, solitaria).
trata(amate, inflamacion).
trata(amate, infecciones_leves).
accion(amate, antiparasitario).
accion(amate, antiinflamatorio).
precaucion(amate, puede_irritar_la_piel).

% ANIS (Pag. 52)
planta(anis).
nombre_cientifico(anis, 'Pimpinella anisum').
origen(anis, asia, region_del_mediterraneo).
modo_preparacion(anis, infusion).
enfermedad(colitis_leve).
enfermedad(indigestion).
enfermedad(flatulencias).
enfermedad(colicos).
enfermedad(tos).
enfermedad(bronquitis).
trata(anis, colitis_leve).
trata(anis, indigestion).
trata(anis, flatulencias).
trata(anis, colicos).
trata(anis, tos).
trata(anis, bronquitis).
accion(anis, carminativo).
accion(anis, expectorante).

% ANACAHUITE (Pag. 53)
planta(anacahuite).
nombre_cientifico(anacahuite, 'Cordia boissieri').
origen(anacahuite, america, mexico).
modo_preparacion(anacahuite, cocimiento).
enfermedad(bronquitis).
enfermedad(tos).
enfermedad(pulmones).
enfermedad(resfriado).
trata(anacahuite, bronquitis).
trata(anacahuite, tos).
trata(anacahuite, pulmones).
trata(anacahuite, resfriado).
accion(anacahuite, expectorante).
accion(anacahuite, balsamico).

% ARNICA (Pag. 54)
planta(arnica).
nombre_cientifico(arnica, 'Arnica montana').
origen(arnica, europa, suiza).
modo_preparacion(arnica, macera).
enfermedad(golpes).
enfermedad(torceduras).
enfermedad(moretones).
trata(arnica, golpes).
trata(arnica, torceduras).
trata(arnica, moretones).
accion(arnica, antiinflamatoria).
accion(arnica, cicatrizante).
accion(arnica, analgesica).

% BARBASCO (Pag. 55)
planta(barbasco).
nombre_cientifico(barbasco, 'Jacquinia arborea').
origen(barbasco, america, mexico).
modo_preparacion(barbasco, cocimiento).
enfermedad(verrugas).
enfermedad(tina).
enfermedad(sarna).
enfermedad(anticonceptivo).
trata(barbasco, verrugas).
trata(barbasco, tina).
trata(barbasco, sarna).
trata(barbasco, anticonceptivo).
accion(barbasco, toxica).
accion(barbasco, irritante).
accion(barbasco, pesticida).

% BELLADONA (Pag. 56)
planta(belladona).
nombre_cientifico(belladona, 'Atropa belladonna').
origen(belladona, europa, europa_central).
modo_preparacion(belladona, macera).
enfermedad(espasmos).
enfermedad(colicos).
enfermedad(dolores_menstruales).
enfermedad(asma).
enfermedad(parkinson).
trata(belladona, espasmos).
trata(belladona, colicos).
trata(belladona, dolores_menstruales).
trata(belladona, asma).
trata(belladona, parkinson).
accion(belladona, antiespasmodica).
accion(belladona, analgesica).
accion(belladona, sedante).
accion(belladona, toxica).

% BERRO (Pag. 57)
planta(berro).
nombre_cientifico(berro, 'Nasturtium officinale').
origen(berro, europa, europa_central).
modo_preparacion(berro, infusion).
enfermedad(bronquitis).
trata(berro, bronquitis).
accion(berro, expectorante).
accion(berro, depurativa).
precaucion(berro, evitar_dosis_altas_puede_ser_irritante).

% BOLDO (Pag. 58)
planta(boldo).
nombre_cientifico(boldo, 'Peumus boldus').
origen(boldo, america, chile).
modo_preparacion(boldo, infusion).
enfermedad(problemas_hepaticos).
trata(boldo, problemas_hepaticos).
accion(boldo, hepatoprotectora).
accion(boldo, colagoga).
precaucion(boldo, evitar_uso_prolongado_embarazo).

% BORRAJA (Pag. 59)
planta(borraja).
nombre_cientifico(borraja, 'Borago officinalis').
origen(borraja, europa, mediterraneo).
modo_preparacion(borraja, infusion).
enfermedad(fiebre).
trata(borraja, fiebre).
accion(borraja, sudorifica).
accion(borraja, antiinflamatoria).
precaucion(borraja, evitar_uso_prolongado_alcaloides_toxicos).

% BUGAMBILIA (Pag. 60)
planta(bugambilia).
nombre_cientifico(bugambilia, 'Bougainvillea glabra').
origen(bugambilia, america, brasil).
modo_preparacion(bugambilia, cocimiento).
enfermedad(tos).
trata(bugambilia, tos).
accion(bugambilia, expectorante).
accion(bugambilia, antitusiva).
precaucion(bugambilia, evitar_en_embarazo_dosis_altas).

% BRIONIA (Pag. 61)
planta(brionia).
nombre_cientifico(brionia, 'Bryonia alba').
origen(brionia, europa, desconocido).
modo_preparacion(brionia, cocimiento).
enfermedad(lombrices).
trata(brionia, lombrices).
accion(brionia, purgante).
accion(brionia, antiparasitaria).
precaucion(brionia, toxica_dosis_altas).

% CANELA (Pag. 62)
planta(canela).
nombre_cientifico(canela, 'Cinnamomum verum').
origen(canela, asia, desconocido).
modo_preparacion(canela, infusion).
enfermedad(anemia).
trata(canela, anemia).
accion(canela, estimulante).
accion(canela, antimicrobiana).
precaucion(canela, alergias_coagulacion).

% CEDRON (Pag. 63)
planta(cedron).
nombre_cientifico(cedron, 'Aloysia citrodora').
origen(cedron, america, desconocido).
modo_preparacion(cedron, infusion).
enfermedad(tos).
trata(cedron, tos).
accion(cedron, expectorante).
accion(cedron, calmante).
precaucion(cedron, presion_arterial_baja).

% CARDO SANTO (Pag. 64)
planta(cardo_santo).
nombre_cientifico(cardo_santo, 'Cnicus benedictus').
origen(cardo_santo, america, mexico).
modo_preparacion(cardo_santo, cocimiento).
enfermedad(nubes_ojos).
trata(cardo_santo, nubes_ojos).
accion(cardo_santo, oftalmica).
precaucion(cardo_santo, contacto_prolongado_ojos).

% CEMPASUCHIL (Pag. 65)
planta(cempasuchil).
nombre_cientifico(cempasuchil, 'Tagetes erecta').
origen(cempasuchil, america, mexico).
modo_preparacion(cempasuchil, te).
enfermedad(parasitos_intestinales).
enfermedad(tumores).
trata(cempasuchil, parasitos_intestinales).
accion(cempasuchil, antiparasitario).
accion(cempasuchil, tonico).
precaucion(cempasuchil, cocimiento_3_veces_al_dia).

% CHAPARRO AMARGOSO (Pag. 66)
planta(chaparro_amargoso).
nombre_cientifico(chaparro_amargoso, 'Castela americana').
origen(chaparro_amargoso, america, mexico).
modo_preparacion(chaparro_amargoso, te).
enfermedad(disenteria_amebiana).
enfermedad(diarrea).
enfermedad(flujo).
enfermedad(hemorragias_internas).
trata(chaparro_amargoso, disenteria_amebiana).
trata(chaparro_amargoso, diarrea).
trata(chaparro_amargoso, flujo).
trata(chaparro_amargoso, hemorragias_internas).
accion(chaparro_amargoso, antiseptico).
precaucion(chaparro_amargoso, 'En disentería crónica usar como lavativa').

% CHICALOTE (Pag. 67)
planta(chicalote).
nombre_cientifico(chicalote, 'Argemone aecholtzia').
origen(chicalote, america, mexico).
modo_preparacion(chicalote, cocimiento).
enfermedad(tos).
enfermedad(asma).
enfermedad(tosferina).
enfermedad(epilepsia).
enfermedad(artritis).
enfermedad(insomnio).
enfermedad(ansiedad).
enfermedad(colicos_hepaticos).
enfermedad(colicos_renales).
enfermedad(colicos_intestinales).
trata(chicalote, tos).
trata(chicalote, asma).
trata(chicalote, tosferina).
trata(chicalote, epilepsia).
trata(chicalote, artritis).
trata(chicalote, insomnio).
trata(chicalote, ansiedad).
trata(chicalote, colicos_hepaticos).
trata(chicalote, colicos_renales).
trata(chicalote, colicos_intestinales).
accion(chicalote, antiespasmodico).
accion(chicalote, hipnotico).
accion(chicalote, sedante).
precaucion(chicalote, 'Es un poco tóxica, usar con cuidado').

% CHILE (Pag. 68)
planta(chile).
nombre_cientifico(chile, 'Capsicum annuum').
origen(chile, america, mexico).
modo_preparacion(chile, compresas_calientes).
enfermedad(asma).
enfermedad(reumatismo).
trata(chile, asma).
trata(chile, reumatismo).
accion(chile, antiinflamatorio).
accion(chile, rubefaciente).
precaucion(chile, 'No debe darse a niños; puede irritar mucosas, diarrear, inflamar hígado y hemorroides').

% CHICHIGUA (Pag. 69)
planta(chichigua).
nombre_cientifico(chichigua, 'Solanum mammosum').
origen(chichigua, america, mexico).
modo_preparacion(chichigua, decoccion).
enfermedad(dermatitis).
enfermedad(inflamacion).
enfermedad(resfriado).
trata(chichigua, dermatitis).
trata(chichigua, inflamacion).
accion(chichigua, antiinflamatorio).
accion(chichigua, antiseptico).
precaucion(chichigua, toxica_si_se_ingiere).

% CILANTRO (Pag. 70)
planta(cilantro).
nombre_cientifico(cilantro, 'Coriandrum sativum').
origen(cilantro, asia, iran).
modo_preparacion(cilantro, infusion).
enfermedad(problemas_digestivos).
enfermedad(ansiedad).
trata(cilantro, problemas_digestivos).
trata(cilantro, ansiedad).
accion(cilantro, carminativo).
accion(cilantro, relajante).
precaucion(cilantro, evitar_en_embarazo_excesivo).

% Pagina 71 cocolmeca
planta(cocolmeca).
nombre_cientifico(cocolmeca, 'Smilax spinosa').
origen(cocolmeca, america, mexico).
modo_preparacion(cocolmeca, decoccion).
enfermedad(reumatismo).
enfermedad(acne).
enfermedad(anemia).
sintoma(reumatismo, dolor_articular).
sintoma(acne, inflamacion_piel).
sintoma(anemia, dolor_pecho).
trata(cocolmeca, reumatismo).
trata(cocolmeca, acne).
accion(cocolmeca, depurativo).
accion(cocolmeca, antiinflamatorio).
modo_tratamiento(cocolmeca, una_vez_dia).
precaucion(cocolmeca, no_usar_por_periodos_prolongados).

% Pagina 72 cola de caballo
planta(cola_de_caballo).
nombre_cientifico(cola_de_caballo, 'Equisetum arvensis').
origen(cola_de_caballo, europa, francia).
modo_preparacion(cola_de_caballo, infusion).
enfermedad(retencion_liquidos).
enfermedad(calculos_renales).
sintoma(retencion_liquidos, hinchazon).
sintoma(calculos_renales, dolor_renal).
trata(cola_de_caballo, retencion_liquidos).
trata(cola_de_caballo, calculos_renales).
accion(cola_de_caballo, diuretico).
accion(cola_de_caballo, remineralizante).
modo_tratamiento(cola_de_caballo, una_a_dos_veces_dia).
precaucion(cola_de_caballo, evitar_en_insuficiencia_renal).

%  Pagina 73 Colchino (Argemone mexicana)
planta(colchino).
nombre_cientifico(colchino, 'Argemone mexicana').
origen(colchino, america, mexico).
modo_preparacion(colchino, infusion_de_hojas).
enfermedad(dolor_estomacal).
enfermedad(parasitos).
enfermedad(problemas_hepaticos).
sintoma(dolor_estomacal, dolor_abdominal).
sintoma(parasitos, comezon_anal).
sintoma(parasitos, dolor_abdominal).
sintoma(problemas_hepaticos, color_amarillo_en_la_piel).
trata(colchino, dolor_estomacal).
trata(colchino, parasitos).
trata(colchino, problemas_hepaticos).
accion(colchino, analgesico).
accion(colchino, antiparasitario).
accion(colchino, hepatoprotector).
tratamiento(colchino, tomar_taza_infusion_dos_veces_al_dia_por_5_dias).
precaucion(colchino, dosis_alta_toxica).

%  Pagina 74 Comino (Cuminum cyminum)
planta(comino).
nombre_cientifico(comino, 'Cuminum cyminum').
origen(comino, asia, desconocido).
modo_preparacion(comino, infusion_de_semillas).
enfermedad(indigestión).
enfermedad(flatulencia).
enfermedad(colico_menstrual).
sintoma(indigestión, pesadez_estomacal).
sintoma(flatulencia, gases).
sintoma(colico_menstrual, dolor_abdominal).
trata(comino, indigestión).
trata(comino, flatulencia).
trata(comino, colico_menstrual).
accion(comino, carminativo).
accion(comino, antiespasmodico).
accion(comino, digestivo).
tratamiento(comino, tomar_taza_infusion_tras_comidas).
precaucion(comino, evitar_en_embarazo).

%  Pagina 75 Colpachi (Croton glabellus)
planta(colpachi).
nombre_cientifico(colpachi, 'Croton glabellus').
origen(colpachi, america, mexico).
modo_preparacion(colpachi, decoccion_de_corteza).
enfermedad(sarampión).
enfermedad(afecciones_de_la_piel).
enfermedad(fiebre).
sintoma(sarampión, fiebre).
sintoma(sarampión, manchas_rojas).
sintoma(afecciones_de_la_piel, irritacion).
sintoma(fiebre, temperatura_alta).
trata(colpachi, sarampión).
trata(colpachi, afecciones_de_la_piel).
trata(colpachi, fiebre).
accion(colpachi, antipiretico).
accion(colpachi, antiseptico).
accion(colpachi, dermatologico).
tratamiento(colpachi, aplicar_infusion_tibia_en_piel_2_veces_al_dia).
precaucion(colpachi, evitar_en_lactancia).

%  Pagina 76 Cuachalalate (Amphipterygium adstringens)
planta(cuachalalate).
nombre_cientifico(cuachalalate, 'Amphipterygium adstringens').
origen(cuachalalate, america, mexico).
modo_preparacion(cuachalalate, infusion_o_decoccion_de_corteza).
enfermedad(ulceras).
enfermedad(infecciones_gastrointestinales).
enfermedad(problemas_bucales).
sintoma(ulceras, dolor_interno).
sintoma(infecciones_gastrointestinales, diarrea).
sintoma(problemas_bucales, inflamacion_encias).
trata(cuachalalate, ulceras).
trata(cuachalalate, infecciones_gastrointestinales).
trata(cuachalalate, problemas_bucales).
accion(cuachalalate, cicatrizante).
accion(cuachalalate, astringente).
accion(cuachalalate, antiinflamatorio).
tratamiento(cuachalalate, tomar_infusion_3_veces_al_dia_tras_las_comidas).
precaucion(cuachalalate, no_uso_prolongado).

% Pagina 77 cuajiote
planta(cuajiote).
nombre_cientifico(cuajiote, 'Bursera morelense').
origen(cuajiote, america, mexico).
modo_preparacion(cuajiote, infusion).
enfermedad(anasarca).
enfermedad(estreñimiento_cronico).
sintoma(anasarca, hinchazon_generalizada).
sintoma(estreñimiento_cronico, estreñimiento).
trata(cuajiote, anasarca).
trata(cuajiote, estreñimiento_cronico).
accion(cuajiote, laxante).
modo_tratamiento(cuajiote, uso_moderado_no_mas_de_3_dias).
precaucion(cuajiote, puede_provocar_gastroenteritis).
precaucion(cuajiote, puede_provocar_rectitis).
precaucion(cuajiote, puede_provocar_enterocolitis).

% Pagina 78 cuasia
planta(cuasia).
nombre_cientifico(cuasia, 'Quassia amara').
origen(cuasia, america, costa_rica).
origen(cuasia, america, nicaragua).
origen(cuasia, america, panama).
origen(cuasia, america, brasil).
origen(cuasia, america, peru).
origen(cuasia, america, venezuela).
origen(cuasia, america, surinam).
origen(cuasia, america, colombia).
origen(cuasia, america, argentina).
origen(cuasia, america, guyana_francesa).
origen(cuasia, america, guyana).
origen(cuasia, america, mexico).
modo_preparacion(cuasia, infusion).
enfermedad(diabetes).
enfermedad(artritis).
enfermedad(reumatismo).
enfermedad(dolor_corporal).
enfermedad(migraña).
enfermedad(dolor_de_estomago).
sintoma(diabetes, mucha_sed).
sintoma(diabetes, fatiga).
sintoma(diabetes, nivel_alto_de_glucosa).
sintoma(artritis, dolor_articular).
sintoma(artritis, inflamacion).
sintoma(reumatismo, rigidez_articular).
sintoma(reumatismo, hinchazon).
sintoma(dolor_corporal, dolor).
sintoma(migraña, dolor_de_cabeza).
sintoma(dolor_de_estomago, nausea).
sintoma(dolor_de_estomago, dolor_de_estomago).
trata(cuasia, diabetes).
trata(cuasia, artritis).
trata(cuasia, reumatismo).
trata(cuasia, dolor_corporal).
trata(cuasia, migraña).
trata(cuasia, dolor_de_estomago).
accion(cuasia, analgesico).
accion(cuasia, antiinflamatorio).
accion(cuasia, hipoglucemiante).
modo_tratamiento(cuasia, una_taza_por_dia).
precaucion(cuasia, puede_provocar_irritacion_gastrica).
precaucion(cuasia, puede_provocar_vomito).
precaucion(cuasia, puede_provocar_estupor).

% Pagina 79 damiana
planta(damiana).
nombre_cientifico(damiana, 'Turnera diffusa').
origen(damiana, america, mexico).
modo_preparacion(damiana, infusion).
enfermedad(hipersexualidad).
enfermedad(alcoholismo).
enfermedad(diabetes).
enfermedad(nefritis).
enfermedad(orquitis).
enfermedad(males_de_la_vejiga).
sintoma(hipersexualidad, ansiedad).
sintoma(hipersexualidad, impulsos_sexuales_intensos).
sintoma(hipersexualidad, carencia_de_control_de_deseos).
sintoma(alcoholismo, ansiedad).
sintoma(alcoholismo, deseo_de_beber).
sintoma(alcoholismo, tolerancia_alcoholica).
sintoma(alcoholismo, perdida_del_control).
sintoma(diabetes, mucha_sed).
sintoma(diabetes, nivel_alto_de_glucosa).
sintoma(nefritis, dolor_en_riñones).
sintoma(nefritis, sangre_en_orina).
sintoma(orquitis, hinchazon_en_testiculos).
sintoma(orquitis, dolor_al_orinar).
sintoma(males_de_la_vejiga, dolor_al_orinar).
sintoma(males_de_la_vejiga, urgencia_frecuente).
trata(damiana, hipersexualidad).
trata(damiana, alcoholismo).
trata(damiana, diabetes).
trata(damiana, nefritis).
trata(damiana, orquitis).
trata(damiana, males_de_la_vejiga).
accion(damiana, relajante).
accion(damiana, diuretico).
accion(damiana, antiinflamatorio).
modo_tratamiento(damiana, de_una_a_dos_tazas_al_dia).
precaucion(damiana, puede_provocar_insomnio).
precaucion(damiana, puede_provocar_irritacion_gastrica).

%Pagina 80 diente_de_leon
planta(diente_de_leon).
nombre_cientifico(diente_de_leon, 'Taraxacum officinale').
origen(diente_de_leon, europa, francia).
origen(diente_de_leon, europa, alemania).
origen(diente_de_leon, europa, españa).
origen(diente_de_leon, europa, italia).
origen(diente_de_leon, europa, reino_unido).
origen(diente_de_leon, europa, rusia).
origen(diente_de_leon, asia, china).
origen(diente_de_leon, asia, japon).
modo_preparacion(diente_de_leon, infusion).
enfermedad(anemia).
enfermedad(acumulacion_de_toxinas).
sintoma(anemia, debilidad).
sintoma(acumulacion_de_toxinas, toxinas_en_riñon).
sintoma(acumulacion_de_toxinas, toxinas_en_higado).
sintoma(acumulacion_de_toxinas, urea_en_sangre).
trata(diente_de_leon, anemia).
trata(diente_de_leon, acumulacion_de_toxinas).
accion(diente_de_leon, aperitivo).
accion(diente_de_leon, depurativo).
accion(diente_de_leon, laxante).
accion(diente_de_leon, colagogo).
accion(diente_de_leon, diuretico).
accion(diente_de_leon, alimenticio).
modo_tratamiento(diente_de_leon, te_tomado_en_ayunas).
precaucion(diente_de_leon, puede_provocar_diarrea).
precaucion(diente_de_leon, puede_provocar_acidez_estomacal).
precaucion(diente_de_leon, puede_provocar_dolor_abdominal).
precaucion(diente_de_leon, puede_provocar_gases).

% Pagina 81.  digitaria
planta(digitaria).
nombre_cientifico(digitaria, 'Digitalis purpurea').
origen(digitaria, europa, desconocido).
modo_preparacion(digitaria, infusion).
enfermedad(miocarditis).
enfermedad(astenia).
enfermedad(epilepsia).
sintoma(miocarditis, dolor_toracico).
sintoma(astenia, fatiga).
sintoma(epilepsia, convulsiones).
trata(digitaria, miocarditis).
trata(digitaria, astenia).
trata(digitaria, epilepsia).
accion(digitaria, cardiotonica).
modo_tratamiento(digitaria, tomar_te_supervision_medica).
precaucion(digitaria, toxica_dosis_altas).

% Pagina 82.  doradilla
planta(doradilla).
nombre_cientifico(doradilla, 'Ceterach officinarum').
origen(doradilla, america, desconocido).
modo_preparacion(doradilla, cocimiento).
enfermedad(nefritis).
sintoma(nefritis, dolor_renal).
trata(doradilla, nefritis).
accion(doradilla, diuretica).
accion(doradilla, desinflamatoria).
modo_tratamiento(doradilla, tomar_te_una_dos_veces_dia).
precaucion(doradilla, evitar_exceso_irritacion_renal).

% Pagina 83.  epazote
planta(epazote).
nombre_cientifico(epazote, 'Dysphania ambrosioides').
origen(epazote, america, desconocido).
modo_preparacion(epazote, cocimiento).
enfermedad(lombrices).
sintoma(lombrices, molestias_abdominales).
trata(epazote, lombrices).
accion(epazote, antiparasitaria).
accion(epazote, carminativa).
modo_tratamiento(epazote, tomar_te_una_vez_dia).
precaucion(epazote, evitar_embarazo_ninos).

% Pagina 84.  enebro
planta(enebro).
nombre_cientifico(enebro, 'Juniperus communis').
origen(enebro, europa, desconocido).
modo_preparacion(enebro, infusion).
enfermedad(leucorrea).
sintoma(leucorrea, flujo_anormal).
trata(enebro, leucorrea).
accion(enebro, antiseptica).
accion(enebro, diuretica).
modo_tratamiento(enebro, tomar_te_o_lavados_externos).
precaucion(enebro, evitar_problemas_renales_embarazo).

% ESTAFIATE (Pag. 85)
planta(estafiate).
nombre_cientifico(estafiate, 'Artemisia ludoviciana').
origen(estafiate, america, mexico).
modo_preparacion(estafiate, infusion).
enfermedad(empacho).
enfermedad(dolor_estomacal).
trata(estafiate, empacho).
trata(estafiate, dolor_estomacal).
accion(estafiate, digestivo).
accion(estafiate, antiparasitario).
precaucion(estafiate, embarazo).

% EUCALIPTO (Pag. 86)
planta(eucalipto).
nombre_cientifico(eucalipto, 'Eucalyptus globulus').
origen(eucalipto, oceania, australia).
modo_preparacion(eucalipto, vaporizacion).
enfermedad(gripe).
enfermedad(resfriado).
trata(eucalipto, gripe).
trata(eucalipto, resfriado).
accion(eucalipto, expectorante).
accion(eucalipto, antiseptico).
precaucion(eucalipto, no_ingerir_aceite_esencial_puro).

% FENOGRECO (Pag. 87)
planta(fenogreco).
nombre_cientifico(fenogreco, 'Trigonella foenum-graecum').
origen(fenogreco, asia, india).
modo_preparacion(fenogreco, cocimiento).
enfermedad(diabetes).
enfermedad(colesterol).
trata(fenogreco, diabetes).
trata(fenogreco, colesterol).
accion(fenogreco, hipoglucemiante).
accion(fenogreco, hipolipemiante).
precaucion(fenogreco, no_usar_en_embarazo).

% GENCIANA (Pag. 88)
planta(genciana).
nombre_cientifico(genciana, 'Gentiana lutea').
origen(genciana, europa, francia).
modo_preparacion(genciana, maceracion).
enfermedad(anemia).
enfermedad(indigestion).
trata(genciana, anemia).
trata(genciana, indigestion).
accion(genciana, estimulante).
accion(genciana, tonico).
precaucion(genciana, evitar_en_ulceras_gastricas).

% GERANIO (Pag. 89)
planta(geranio).
nombre_cientifico(geranio, 'Pelargonium graveolens').
origen(geranio, africa, sudafrica).
modo_preparacion(geranio, infusion).
enfermedad(estres).
enfermedad(insomnio).
trata(geranio, estres).
trata(geranio, insomnio).
accion(geranio, relajante).
accion(geranio, ansiolitico).
precaucion(geranio, posible_irritacion_cutanea).

% GIRASOL (Pag. 90)
planta(girasol).
nombre_cientifico(girasol, 'Helianthus annuus').
origen(girasol, america, mexico).
modo_preparacion(girasol, infusion).
enfermedad(fiebre).
enfermedad(presion_alta).
trata(girasol, fiebre).
trata(girasol, presion_alta).
accion(girasol, diuretico).
accion(girasol, antiinflamatorio).
precaucion(girasol, exceso).

% GINSENG (Pag. 91)
planta(ginseng).
nombre_cientifico(ginseng, 'Panax ginseng').
origen(ginseng, asia, china).
modo_preparacion(ginseng, infusion).
enfermedad(fatiga).
enfermedad(diabetes).
trata(ginseng, fatiga).
trata(ginseng, diabetes).
accion(ginseng, estimulante).
accion(ginseng, adaptogeno).
precaucion(ginseng, hipertension).
precaucion(ginseng, embarazo).

% GORDOLOBO (Pag. 92)
planta(gordolobo).
nombre_cientifico(gordolobo, 'Verbascum thapsus').
origen(gordolobo, europa, alemania).
modo_preparacion(gordolobo, infusion).
enfermedad(tos).
enfermedad(bronquitis).
trata(gordolobo, tos).
trata(gordolobo, bronquitis).
accion(gordolobo, expectorante).
accion(gordolobo, antiinflamatorio).
precaucion(gordolobo, infusion).

% GRAMA (Pag. 93)
planta(grama).
nombre_cientifico(grama, 'Cynodon dactylon').
origen(grama, africa, egipto).
modo_preparacion(grama, coccion).
enfermedad(infecciones_urinarias).
enfermedad(cistitis).
trata(grama, infecciones_urinarias).
trata(grama, cistitis).
accion(grama, diuretico).
accion(grama, depurativo).
precaucion(grama, embarazo).

% GRANADO (Pag. 94)
planta(granado).
nombre_cientifico(granado, 'Punica granatum').
origen(granado, asia, india).
modo_preparacion(granado, infusion).
enfermedad(parasitos_intestinales).
enfermedad(disenteria).
trata(granado, parasitos_intestinales).
trata(granado, disenteria).
accion(granado, astringente).
accion(granado, antiparasitario).
precaucion(granado, estrenimiento).

% GUACO (Pag. 95)
planta(guaco).
nombre_cientifico(guaco, 'Aristolochia odoratissima').
modo_preparacion(guaco, infusion_cataplasma).
enfermedad(alergia).
enfermedad(vitiligo).
enfermedad(asma).
trata(guaco, alergia).
trata(guaco, vitiligo).
trata(guaco, asma).
accion(guaco, antiinflamatorio).
accion(guaco, broncodilatador).
precaucion(guaco, uso_prolongado).

% GUAZUMA (Pag. 96)
planta(guazuma).
nombre_cientifico(guazuma, 'Guazuma ulmifolia').
modo_preparacion(guazuma, infusion_hojas).
enfermedad(disenteria).
enfermedad(diarrea).
enfermedad(inflamacion_intestinal).
trata(guazuma, disenteria).
trata(guazuma, diarrea).
trata(guazuma, inflamacion_intestinal).
accion(guazuma, antidiarreico).
accion(guazuma, antiinflamatorio).
precaucion(guazuma, alergia_hojas).

% GUAYACAN (Pag. 97)
planta(guayacan).
nombre_cientifico(guayacan, 'Guaiacum sanctum').
modo_preparacion(guayacan, infusion_flores_corteza).
enfermedad(tos).
enfermedad(tuberculosis).
enfermedad(sifilis).
enfermedad(reumatismo).
trata(guayacan, tos).
trata(guayacan, tuberculosis).
trata(guayacan, sifilis).
trata(guayacan, reumatismo).
accion(guayacan, expectorante).
accion(guayacan, antiesifilitico).
precaucion(guayacan, interaccion_medicamentos).

% HAMAMELIS (Pag. 98)
planta(hamamelis).
nombre_cientifico(hamamelis, 'Hamamelis virginica').
modo_preparacion(hamamelis, infusion_hojas).
enfermedad(hemorroides).
enfermedad(varices).
enfermedad(retencion_orina).
trata(hamamelis, hemorroides).
trata(hamamelis, varices).
trata(hamamelis, retencion_orina).
accion(hamamelis, venotonico).
accion(hamamelis, diuretico).
precaucion(hamamelis, hipersensibilidad).

% HELENIO (Pag. 99)
planta(helenio).
nombre_cientifico(helenio, 'Inula helenium').
modo_preparacion(helenio, infusion_raiz).
enfermedad(bronquitis).
enfermedad(tos_ferina).
enfermedad(retencion_orina).
trata(helenio, bronquitis).
trata(helenio, tos_ferina).
trata(helenio, retencion_orina).
accion(helenio, expectorante).
accion(helenio, diuretico).
precaucion(helenio, dosis_excesiva).

% JENGIBRE (Pag. 105)
planta(jengibre).
nombre_cientifico(jengibre, 'Zingiber officinale').
modo_preparacion(jengibre, tintura).
modo_preparacion(jengibre, pan).
modo_preparacion(jengibre, remedio_efervescente).
enfermedad(cruda).
enfermedad(agotamiento_mental).
trata(jengibre, cruda).
trata(jengibre, agotamiento_mental).
accion(jengibre, estimulante).
accion(jengibre, digestiva).
precaucion(jengibre, hipertension).

% LINAZA (Pag. 106)
planta(linaza).
nombre_cientifico(linaza, 'Linum usitatissimum').
modo_preparacion(linaza, infusion).
modo_preparacion(linaza, cataplasma).
enfermedad(estrenimiento).
enfermedad(colitis).
enfermedad(males_estomacales).
enfermedad(bronquitis).
enfermedad(hemorroides).
enfermedad(heridas).
enfermedad(abscesos).
trata(linaza, estrenimiento).
trata(linaza, colitis).
trata(linaza, males_estomacales).
trata(linaza, bronquitis).
trata(linaza, hemorroides).
trata(linaza, heridas).
trata(linaza, abscesos).

% Pagina 107 --- LLANTEN ---
planta(llanten).
nombre_cientifico(llanten, 'Plantago major').
continente_origen(llanten, desconocido).
pais_origen(llanten, desconocido).
parte_util(llanten, hojas).
modo_preparacion(llanten, infusion).
modo_preparacion(llanten, leche_hervida).
modo_tratamiento(llanten, uso_externo).
modo_tratamiento(llanten, uso_interno).
enfermedad(conjuntivitis).
enfermedad(infeccion_ojos).
enfermedad(ulceras_boca).
enfermedad(pequenas_infecciones).
enfermedad(disenteria).
enfermedad(enterocolitis).
sintoma_enfermedad(conjuntivitis, ojos_rojos).
sintoma_enfermedad(infeccion_ojos, secrecion).
sintoma_enfermedad(ulceras_boca, lesiones_bucales).
sintoma_enfermedad(disenteria, diarrea_con_sangre).
sintoma_enfermedad(enterocolitis, dolor_abdominal).
trata_enfermedad(llanten, conjuntivitis).
trata_enfermedad(llanten, infeccion_ojos).
trata_enfermedad(llanten, ulceras_boca).
trata_enfermedad(llanten, pequenas_infecciones).
trata_enfermedad(llanten, disenteria).
trata_enfermedad(llanten, enterocolitis).
accion_efecto_planta(llanten, antiinflamatoria).
accion_efecto_planta(llanten, astringente).

% Pagina 108 --- MADRESELVA ---
planta(madreselva).
nombre_cientifico(madreselva, 'Lonicera').
continente_origen(madreselva, desconocido).
pais_origen(madreselva, desconocido).
parte_util(madreselva, flores).
modo_preparacion(madreselva, infusion).
modo_tratamiento(madreselva, uso_interno).
enfermedad(gripa).
enfermedad(tos).
enfermedad(infecciones_garganta).
sintoma_enfermedad(gripa, fiebre_y_congestion).
sintoma_enfermedad(tos, irritacion_garganta).
sintoma_enfermedad(infecciones_garganta, dolor_garganta).
trata_enfermedad(madreselva, gripa).
trata_enfermedad(madreselva, tos).
trata_enfermedad(madreselva, infecciones_garganta).
accion_efecto_planta(madreselva, expectorante).
accion_efecto_planta(madreselva, antibacteriana).

% Pagina 109 --- MAGUEY ---
planta(maguey).
nombre_cientifico(maguey, 'Agave').
continente_origen(maguey, america).
pais_origen(maguey, mexico).
parte_util(maguey, hojas).
modo_preparacion(maguey, cataplasma).
modo_tratamiento(maguey, uso_externo).
enfermedad(llagas).
enfermedad(infecciones_piel).
enfermedad(fiebre).
sintoma_enfermedad(llagas, heridas_abiertas).
sintoma_enfermedad(infecciones_piel, enrojecimiento).
sintoma_enfermedad(fiebre, temperatura_alta).
trata_enfermedad(maguey, llagas).
trata_enfermedad(maguey, infecciones_piel).
trata_enfermedad(maguey, fiebre).
accion_efecto_planta(maguey, cicatrizante).
accion_efecto_planta(maguey, antimicrobiana).
accion_efecto_planta(maguey, antipiretica).

% Pagina 100 --- HIERBA DEL POLLO ---
planta(hierba_del_pollo).
nombre_cientifico(hierba_del_pollo, 'Tradescantia zebrina').
continente_origen(hierba_del_pollo, america).
pais_origen(hierba_del_pollo, mexico).
modo_preparacion(hierba_del_pollo, machacar).
modo_preparacion(hierba_del_pollo, infusion).
enfermedad(hemorragia).
enfermedad(problemas_renales).
sintoma_enfermedad(hemorragia, sangrado).
trata_enfermedad(hierba_del_pollo, hemorragia).
trata_enfermedad(hierba_del_pollo, problemas_renales).
accion_efecto_planta(hierba_del_pollo, hemostatico).
accion_efecto_planta(hierba_del_pollo, cicatrizante).
accion_efecto_planta(hierba_del_pollo, diuretico).
modo_tratamiento(hierba_del_pollo, aplicacion_local).
modo_tratamiento(hierba_del_pollo, tomar_te).
precaucion_planta(hierba_del_pollo, embarazo).

% Pagina 101 --- HINOJO ---
planta(hinojo).
nombre_cientifico(hinojo, 'Foeniculum vulgare').
continente_origen(hinojo, europa).
pais_origen(hinojo, mexico).
modo_preparacion(hinojo, infusion).
enfermedad(gases).
enfermedad(flatulencias).
enfermedad(obstruccion_mucosa_pecho).
sintoma_enfermedad(gases, dolor_abdominal).
sintoma_enfermedad(flatulencias, distension_abdominal).
trata_enfermedad(hinojo, gases).
trata_enfermedad(hinojo, flatulencias).
accion_efecto_planta(hinojo, digestivo).
modo_tratamiento(hinojo, tomar_te).
precaucion_planta(hinojo, embarazo).

% Pagina 102 --- JALAPA ---
planta(jalapa).
nombre_cientifico(jalapa, 'Ipomea purga').
continente_origen(jalapa, america).
pais_origen(jalapa, mexico).
modo_preparacion(jalapa, cocimiento).
enfermedad(disentiria).
enfermedad(estreñimiento).
enfermedad(indigestion).
enfermedad(apoplejia).
enfermedad(congestion_cerebral).
sintoma_enfermedad(disentiria, diarrea).
sintoma_enfermedad(estreñimiento, dificultad_defecacion).
sintoma_enfermedad(indigestion, malestar_abdominal).
trata_enfermedad(jalapa, disentiria).
trata_enfermedad(jalapa, estreñimiento).
trata_enfermedad(jalapa, indigestion).
trata_enfermedad(jalapa, apoplejia).
trata_enfermedad(jalapa, congestion_cerebral).
accion_efecto_planta(jalapa, purgante).
modo_tratamiento(jalapa, tomar_en_ayunas).

% Pagina 103 --- IPECACUANA ---
planta(ipecacuana).
nombre_cientifico(ipecacuana, 'Polygala hondurana').
continente_origen(ipecacuana, america).
pais_origen(ipecacuana, mexico).
modo_preparacion(ipecacuana, infusion).
enfermedad(tos).
sintoma_enfermedad(tos, tos_seca).
trata_enfermedad(ipecacuana, tos).
accion_efecto_planta(ipecacuana, expectorante).
modo_tratamiento(ipecacuana, tomar_infusion).

% Pagina 104 --- JAZMIN AMARILLO ---
planta(jazmin_amarillo).
nombre_cientifico(jazmin_amarillo, 'Gelsemium sempervirens').
continente_origen(jazmin_amarillo, america).
pais_origen(jazmin_amarillo, mexico).
modo_preparacion(jazmin_amarillo, tintura).
enfermedad(dolores_de_cabeza).
enfermedad(reuma).
enfermedad(espasmos).
enfermedad(asmo_bronquial).
enfermedad(menstruacion_dolorosa).
sintoma_enfermedad(dolores_de_cabeza, dolor_intenso).
sintoma_enfermedad(reuma, dolor_articular).
sintoma_enfermedad(espasmos, contraccion_muscular).
sintoma_enfermedad(asmo_bronquial, dificultad_respiratoria).
trata_enfermedad(jazmin_amarillo, dolores_de_cabeza).
trata_enfermedad(jazmin_amarillo, reuma).
trata_enfermedad(jazmin_amarillo, espasmos).
trata_enfermedad(jazmin_amarillo, asma_bronquial).
trata_enfermedad(jazmin_amarillo, menstruacion_dolorosa).
accion_efecto_planta(jazmin_amarillo, analgesico).
accion_efecto_planta(jazmin_amarillo, antiespasmodico).
precaucion_planta(jazmin_amarillo, problemas_corazon).
precaucion_planta(jazmin_amarillo, problemas_riñones).
modo_tratamiento(jazmin_amarillo, tomar_tintura).

% Pagina 110 --- MAIZ ---
planta(maiz).
nombre_cientifico(maiz, 'Zea mays').
continente_origen(maiz, america).
pais_origen(maiz, mexico).
modo_preparacion(maiz, infusion).
modo_preparacion(maiz, cataplasma).
modo_preparacion(maiz, consumo_directo).
enfermedad(problemas_renales).
enfermedad(hipertension).
enfermedad(problemas_digestivos).
enfermedad(inflamacion).
enfermedad(diabetes).
sintoma_enfermedad(problemas_renales, dolor_espalda_baja).
sintoma_enfermedad(hipertension, presion_alta).
sintoma_enfermedad(problemas_digestivos, dolor_estomago).
sintoma_enfermedad(inflamacion, hinchazon).
sintoma_enfermedad(diabetes, niveles_azucar_altos).
trata_enfermedad(maiz, problemas_renales).
trata_enfermedad(maiz, hipertension).
trata_enfermedad(maiz, problemas_digestivos).
trata_enfermedad(maiz, inflamacion).
trata_enfermedad(maiz, diabetes).
accion_efecto_planta(maiz, diuretico).
accion_efecto_planta(maiz, antiinflamatorio).
accion_efecto_planta(maiz, regulador_glucemia).
modo_tratamiento(maiz, tomar_te_maiz_tres_veces_dia).
modo_tratamiento(maiz, aplicar_cataplasma_maiz).
modo_tratamiento(maiz, consumir_maiz_hervido).
precaucion_planta(maiz, moderar_consumo_en_diabeticos).
precaucion_planta(maiz, evitar_exceso_si_toma_diureticos).

% Pagina 111 --- MALVA ---
planta(malva).
nombre_cientifico(malva, 'Malva sylvestris').
continente_origen(malva, europa).
pais_origen(malva, region_mediterranea).
modo_preparacion(malva, infusion).
modo_preparacion(malva, cataplasma).
modo_preparacion(malva, gargaras).
enfermedad(irritacion_garganta).
enfermedad(estrenimiento).
enfermedad(problemas_piel).
enfermedad(ulceras).
enfermedad(inflamacion_bucal).
sintoma_enfermedad(irritacion_garganta, dolor_al_tragar).
sintoma_enfermedad(estrenimiento, dificultad_defecar).
sintoma_enfermedad(problemas_piel, erupciones_cutaneas).
sintoma_enfermedad(ulceras, llagas_estomacales).
sintoma_enfermedad(inflamacion_bucal, encias_hinchadas).
trata_enfermedad(malva, irritacion_garganta).
trata_enfermedad(malva, estrenimiento).
trata_enfermedad(malva, problemas_piel).
trata_enfermedad(malva, ulceras).
trata_enfermedad(malva, inflamacion_bucal).
accion_efecto_planta(malva, emoliente).
accion_efecto_planta(malva, antiinflamatorio).
accion_efecto_planta(malva, laxante_suave).
accion_efecto_planta(malva, cicatrizante).
modo_tratamiento(malva, tomar_infusion_tres_veces_dia).
modo_tratamiento(malva, aplicar_cataplasma_zonas_afectadas).
modo_tratamiento(malva, hacer_gargaras_dos_veces_dia).
precaucion_planta(malva, no_consumir_en_embarazo).
precaucion_planta(malva, moderar_uso_en_ninos).
precaucion_planta(malva, evitar_si_alergia_malvaceas).

% Pagina 112 --- MALVAVISCO ---
planta(malvavisco).
nombre_cientifico(malvavisco, 'Althaea officinalis').
continente_origen(malvavisco, europa).
pais_origen(malvavisco, 'Europa Oriental').
modo_preparacion(malvavisco, infusion_raiz).
modo_preparacion(malvavisco, jarabe).
modo_preparacion(malvavisco, cataplasma_hojas).
enfermedad(bronquitis).
enfermedad(gastritis).
enfermedad(eczema).
enfermedad(faringitis).
enfermedad(ciatica).
sintoma_enfermedad(bronquitis, tos_con_flema).
sintoma_enfermedad(gastritis, acidez_estomacal).
sintoma_enfermedad(eczema, picor_piel).
sintoma_enfermedad(faringitis, dolor_garganta).
sintoma_enfermedad(ciatica, dolor_pierna).
trata_enfermedad(malvavisco, bronquitis).
trata_enfermedad(malvavisco, gastritis).
trata_enfermedad(malvavisco, eczema).
trata_enfermedad(malvavisco, faringitis).
trata_enfermedad(malvavisco, ciatica).
accion_efecto_planta(malvavisco, demulcente).
accion_efecto_planta(malvavisco, antitusivo).
accion_efecto_planta(malvavisco, antiacido).
accion_efecto_planta(malvavisco, emoliente_intenso).
modo_tratamiento(malvavisco, tomar_jarabe_4_veces_dia).
modo_tratamiento(malvavisco, infusion_raiz_fria).
modo_tratamiento(malvavisco, compresas_raiz_molida).

% Pagina 113 --- MANGLE ---
planta(mangle).
nombre_cientifico(mangle, 'Rhizophora mangle').
familia(mangle, 'Rhizophoraceae').
continente_origen(mangle, america).
region_origen(mangle, 'zonas_intermareales_tropicales').
parte_utilizada(mangle, corteza).
parte_utilizada(mangle, hojas).
parte_utilizada(mangle, propagulos).
modo_preparacion(mangle, decoccion_corteza).
modo_preparacion(mangle, polvo_corteza).
modo_preparacion(mangle, cataplasma_hojas).
enfermedad(leishmaniasis).
enfermedad(diarrea_cronica).
enfermedad(hemorragias).
enfermedad(infecciones_cutaneas).
enfermedad(hepatitis).
sintoma_enfermedad(leishmaniasis, ulceras_piel).
sintoma_enfermedad(diarrea_cronica, deshidratacion).
sintoma_enfermedad(hemorragias, sangrado_excesivo).
sintoma_enfermedad(infecciones_cutaneas, pus_piel).
sintoma_enfermedad(hepatitis, ictericia).
trata_enfermedad(mangle, leishmaniasis).
trata_enfermedad(mangle, diarrea_cronica).
trata_enfermedad(mangle, hemorragias).
trata_enfermedad(mangle, infecciones_cutaneas).
trata_enfermedad(mangle, hepatitis).
accion_efecto_planta(mangle, astringente_intenso).
accion_efecto_planta(mangle, antiviral).
accion_efecto_planta(mangle, hemostatico).
accion_efecto_planta(mangle, cicatrizante).
accion_efecto_planta(mangle, hepatoprotector).
modo_tratamiento(mangle, decoccion_50g_corteza_1litro).
modo_tratamiento(mangle, aplicar_polvo_heridas).
modo_tratamiento(mangle, masticar_propagulos_diarrea).
precaucion_planta(mangle, no_uso_prolongado).
precaucion_planta(mangle, evitar_en_embarazo).
precaucion_planta(mangle, maximo_7dias_continuos).
precaucion_planta(mangle, contraindicado_anemia).

% Pagina 114 --- MANZANILLA ---
planta(manzanilla).
nombre_cientifico(manzanilla, 'Matricaria chamomilla').
familia(manzanilla, 'Asteraceae').
continente_origen(manzanilla, europa).
pais_origen(manzanilla, 'Region Mediterranea').
parte_utilizada(manzanilla, flores).
modo_preparacion(manzanilla, infusion_flores).
modo_preparacion(manzanilla, aceite_esencial).
modo_preparacion(manzanilla, compresas).
modo_preparacion(manzanilla, tintura).
enfermedad(ansiedad).
enfermedad(insomnio).
enfermedad(indigestion).
enfermedad(conjuntivitis).
enfermedad(eczema).
enfermedad(colico_menstrual).
sintoma_enfermedad(ansiedad, nerviosismo).
sintoma_enfermedad(insomnio, dificultad_dormir).
sintoma_enfermedad(indigestion, pesadez_estomacal).
sintoma_enfermedad(conjuntivitis, ojos_rojos).
sintoma_enfermedad(eczema, picazon_piel).
sintoma_enfermedad(colico_menstrual, dolor_uterino).
trata_enfermedad(manzanilla, ansiedad).
trata_enfermedad(manzanilla, insomnio).
trata_enfermedad(manzanilla, indigestion).
trata_enfermedad(manzanilla, conjuntivitis).
trata_enfermedad(manzanilla, eczema).
trata_enfermedad(manzanilla, colico_menstrual).
accion_efecto_planta(manzanilla, sedante_suave).
accion_efecto_planta(manzanilla, antiespasmodico).
accion_efecto_planta(manzanilla, antiinflamatorio).
accion_efecto_planta(manzanilla, carminativo).
accion_efecto_planta(manzanilla, emenagogo).
modo_tratamiento(manzanilla, infusion_3g_flores_150ml).
modo_tratamiento(manzanilla, compresas_ojos_inflamados).
modo_tratamiento(manzanilla, aceite_diluido_masajes).
modo_tratamiento(manzanilla, tintura_30gotas_agua).
precaucion_planta(manzanilla, evitar_alergicos_asteraceas).
precaucion_planta(manzanilla, no_uso_ocular_sin_filtrar).
precaucion_planta(manzanilla, moderar_en_embarazo).
precaucion_planta(manzanilla, no_hervir_flores).

% Pagina 115 --- MARRUBIO ---
planta(marrubio).
nombre_cientifico(marrubio, 'Marrubium vulgare').
continente_origen(marrubio, europa).
pais_origen(marrubio, desconocido).
modo_preparacion(marrubio, infusion).
enfermedad(asma).
enfermedad(caida_cabello).
enfermedad(obesidad).
enfermedad(tos).
enfermedad(reumatismo).
enfermedad(vomitos).
sintoma_enfermedad(asma, dificultad_respiratoria).
sintoma_enfermedad(caida_cabello, perdida_cabello).
sintoma_enfermedad(obesidad, aumento_peso).
sintoma_enfermedad(tos, irritacion_garganta).
sintoma_enfermedad(reumatismo, dolor_articular).
sintoma_enfermedad(vomitos, nausea).
trata_enfermedad(marrubio, asma).
trata_enfermedad(marrubio, caida_cabello).
trata_enfermedad(marrubio, obesidad).
trata_enfermedad(marrubio, tos).
trata_enfermedad(marrubio, reumatismo).
trata_enfermedad(marrubio, vomitos).
accion_efecto_planta(marrubio, expectorante).
accion_efecto_planta(marrubio, digestiva).
modo_tratamiento(marrubio, tomar_te_dos_veces_dia).
precaucion_planta(marrubio, evitar_exceso_molestias_estomacales).

% Pagina 116 --- MARIHUANA ---
planta(marihuana).
nombre_cientifico(marihuana, 'Cannabis sativa').
continente_origen(marihuana, asia).
pais_origen(marihuana, india).
modo_preparacion(marihuana, maceracion).
enfermedad(cancer).
enfermedad(glaucoma).
enfermedad(males_ojos).
enfermedad(artritis).
enfermedad(reumatismo).
sintoma_enfermedad(cancer, dolor_cronico).
sintoma_enfermedad(glaucoma, presion_intraocular).
sintoma_enfermedad(males_ojos, irritacion_ocular).
sintoma_enfermedad(artritis, dolor_articular).
sintoma_enfermedad(reumatismo, dolor_articular).
trata_enfermedad(marihuana, cancer).
trata_enfermedad(marihuana, glaucoma).
trata_enfermedad(marihuana, males_ojos).
trata_enfermedad(marihuana, artritis).
trata_enfermedad(marihuana, reumatismo).
accion_efecto_planta(marihuana, analgesica).
accion_efecto_planta(marihuana, antiinflamatoria).
modo_tratamiento(marihuana, frotaciones_externas_agua_uso).
precaucion_planta(marihuana, supervision_medica_menores_edad).

% Pagina 117 --- MASTUERZO ---
planta(mastuerzo).
nombre_cientifico(mastuerzo, 'Lepidium sativum').
continente_origen(mastuerzo, asia).
pais_origen(mastuerzo, desconocido).
modo_preparacion(mastuerzo, cocimiento).
enfermedad(ciatica).
enfermedad(tuberculosis).
sintoma_enfermedad(ciatica, dolor_nervio).
sintoma_enfermedad(tuberculosis, tos_cronica).
trata_enfermedad(mastuerzo, ciatica).
trata_enfermedad(mastuerzo, tuberculosis).
accion_efecto_planta(mastuerzo, antiinflamatoria).
accion_efecto_planta(mastuerzo, expectorante).
modo_tratamiento(mastuerzo, tomar_te_o_compresas).
precaucion_planta(mastuerzo, tuberculosis_avanzada).

% Pagina 118 --- MATARIQUE ---
planta(matarique).
nombre_cientifico(matarique, 'Psacalium decompositum').
continente_origen(matarique, america).
pais_origen(matarique, mexico).
modo_preparacion(matarique, cocimiento).
enfermedad(diabetes).
enfermedad(reumatismo).
enfermedad(rinones_adoloridos).
sintoma_enfermedad(diabetes, hiperglucemia).
sintoma_enfermedad(reumatismo, dolor_articular).
sintoma_enfermedad(rinones_adoloridos, dolor_renal).
trata_enfermedad(matarique, diabetes).
trata_enfermedad(matarique, reumatismo).
trata_enfermedad(matarique, rinones_adoloridos).
accion_efecto_planta(matarique, desinflamatoria).
accion_efecto_planta(matarique, hipoglucemiante).
modo_tratamiento(matarique, tomar_te_o_tintura).
precaucion_planta(matarique, diabetes_problemas_renales).

% Pagina 119 --- MENTA ---
planta(menta).
nombre_cientifico(menta, 'Mentha piperita').
continente_origen(menta, europa).
pais_origen(menta, desconocido).
modo_preparacion(menta, infusion).
enfermedad(insomnio).
enfermedad(lactancia).
enfermedad(nauseas).
enfermedad(neuralgias).
enfermedad(vomitos).
enfermedad(sarna).
sintoma_enfermedad(insomnio, dificultad_dormir).
sintoma_enfermedad(lactancia, baja_produccion_leche).
sintoma_enfermedad(nauseas, malestar_estomacal).
sintoma_enfermedad(neuralgias, dolor_nervioso).
sintoma_enfermedad(vomitos, nausea).
sintoma_enfermedad(sarna, picazon_piel).
trata_enfermedad(menta, insomnio).
trata_enfermedad(menta, lactancia).
trata_enfermedad(menta, nauseas).
trata_enfermedad(menta, neuralgias).
trata_enfermedad(menta, vomitos).
trata_enfermedad(menta, sarna).
accion_efecto_planta(menta, digestiva).
accion_efecto_planta(menta, calmante).
accion_efecto_planta(menta, refrescante).
modo_tratamiento(menta, tomar_te_dos_tres_veces_dia).
precaucion_planta(menta, reflujo_gastroesofagico).

% Pagina 125 --- OREGANO ---
planta(oregano).
nombre_cientifico(oregano, 'Origanum vulgare').
continente_origen(oregano, europa).
pais_origen(oregano, region_mediterranea).
modo_preparacion(oregano, infusion).
trata_enfermedad(oregano, problemas_digestivos).
trata_enfermedad(oregano, resfriado).
accion_efecto_planta(oregano, antiespasmodico).
accion_efecto_planta(oregano, expectorante).
modo_tratamiento(oregano, dos_veces_dia).
precaucion_planta(oregano, evitar_dosis_altas).

% Pagina 126 --- PALO DE FLOR ---
planta(palo_de_flor).
nombre_cientifico(palo_de_flor, 'Bourreria huanita').
continente_origen(palo_de_flor, america).
pais_origen(palo_de_flor, mexico).
modo_preparacion(palo_de_flor, cocimiento).
trata_enfermedad(palo_de_flor, fiebre).
trata_enfermedad(palo_de_flor, dolor_de_cabeza).
accion_efecto_planta(palo_de_flor, febrifugo).
accion_efecto_planta(palo_de_flor, analgesico).
modo_tratamiento(palo_de_flor, dos_veces_dia).
precaucion_planta(palo_de_flor, no_prolongar_tratamiento).

% Pagina 127 --- PASIFLORA ---
planta(pasiflora).
nombre_cientifico(pasiflora, 'Passiflora incarnata').
continente_origen(pasiflora, america).
pais_origen(pasiflora, mexico).
modo_preparacion(pasiflora, infusion).
trata_enfermedad(pasiflora, insomnio).
trata_enfermedad(pasiflora, ansiedad).
accion_efecto_planta(pasiflora, sedante).
accion_efecto_planta(pasiflora, ansiolitico).
modo_tratamiento(pasiflora, antes_de_dormir).
precaucion_planta(pasiflora, no_combinar_con_alcohol).

% Pagina 128 --- PERICON ---
planta(pericon).
nombre_cientifico(pericon, 'Tagetes lucida').
continente_origen(pericon, america).
pais_origen(pericon, mexico).
modo_preparacion(pericon, infusion).
trata_enfermedad(pericon, colicos).
trata_enfermedad(pericon, indigestion).
accion_efecto_planta(pericon, digestivo).
accion_efecto_planta(pericon, carminativo).
modo_tratamiento(pericon, despues_comidas).
precaucion_planta(pericon, evitar_embarazo).

% Pagina 129 --- PINGUICA ---
planta(pinguica).
nombre_cientifico(pinguica, 'Arctostaphylos pungens').
continente_origen(pinguica, america).
pais_origen(pinguica, mexico).
modo_preparacion(pinguica, cocimiento).
trata_enfermedad(pinguica, infeccion_urinaria).
trata_enfermedad(pinguica, rinones).
accion_efecto_planta(pinguica, diuretico).
accion_efecto_planta(pinguica, depurativo).
modo_tratamiento(pinguica, tres_veces_dia).
precaucion_planta(pinguica, no_usar_por_periodos_largos).

% Pagina 140 --- RUDA ---
planta(ruda).
nombre_cientifico(ruda, 'Ruta graveolens').
continente_origen(ruda, europa).
pais_origen(ruda, espana).
modo_preparacion(ruda, infusion).
enfermedad(menstruacion).
enfermedad(colicos).
enfermedad(nerviosismo).
sintoma_enfermedad(menstruacion, retraso_menstrual).
sintoma_enfermedad(colicos, dolor_abdominal).
sintoma_enfermedad(nerviosismo, ansiedad).
trata_enfermedad(ruda, menstruacion).
trata_enfermedad(ruda, colicos).
trata_enfermedad(ruda, nerviosismo).
accion_efecto_planta(ruda, calmante).
accion_efecto_planta(ruda, emenagoga).
accion_efecto_planta(ruda, antiespasmodica).
modo_tratamiento(ruda, con_moderacion).
precaucion_planta(ruda, evitar_embarazo).

% Pagina 141 --- RUIBARBO ---
planta(ruibarbo).
nombre_cientifico(ruibarbo, 'Rheum palmatum').
continente_origen(ruibarbo, asia).
pais_origen(ruibarbo, china).
modo_preparacion(ruibarbo, cocimiento).
enfermedad(estrenimiento).
enfermedad(digestion).
enfermedad(higado).
sintoma_enfermedad(estrenimiento, dificultad_evacuacion).
sintoma_enfermedad(digestion, pesadez_estomacal).
sintoma_enfermedad(higado, insuficiencia_hepatica).
trata_enfermedad(ruibarbo, estrenimiento).
trata_enfermedad(ruibarbo, digestion).
trata_enfermedad(ruibarbo, higado).
accion_efecto_planta(ruibarbo, laxante).
accion_efecto_planta(ruibarbo, digestiva).
accion_efecto_planta(ruibarbo, tonica).
modo_tratamiento(ruibarbo, pequenas_dosis).
precaucion_planta(ruibarbo, no_usar_en_embarazo).

% Página 142 --- SALVIA ---
planta(salvia).
nombre_cientifico(salvia, 'Salvia officinalis').
continente_origen(salvia, europa).
pais_origen(salvia, italia).
modo_preparacion(salvia, infusion).
enfermedad(dolor_garganta).
enfermedad(digestion).
enfermedad(sudoracion_excesiva).
sintoma_enfermedad(dolor_garganta, inflamacion_faringea).
sintoma_enfermedad(digestion, pesadez_estomacal).
sintoma_enfermedad(sudoracion_excesiva, sudor_constante).
trata_enfermedad(salvia, dolor_garganta).
trata_enfermedad(salvia, digestion).
trata_enfermedad(salvia, sudoracion_excesiva).
accion_efecto_planta(salvia, astringente).
accion_efecto_planta(salvia, digestiva).
accion_efecto_planta(salvia, antiseptica).
modo_tratamiento(salvia, hacer_gargaras_o_beber).
precaucion_planta(salvia, no_usar_en_grandes_cantidades).

% Página 143 --- SEN ---
planta(sen).
nombre_cientifico(sen, 'Senna alexandrina').
continente_origen(sen, africa).
pais_origen(sen, egipto).
modo_preparacion(sen, infusion).
enfermedad(estrenimiento).
sintoma_enfermedad(estrenimiento, dificultad_evacuacion).
trata_enfermedad(sen, estrenimiento).
accion_efecto_planta(sen, laxante).
modo_tratamiento(sen, una_taza_diaria_por_una_semana).
precaucion_planta(sen, no_uso_prolongado).

% Página 144 --- SANGUINARIA ---
planta(sanguinaria).
nombre_cientifico(sanguinaria, 'Sanguinaria canadensis').
continente_origen(sanguinaria, america).
pais_origen(sanguinaria, estados_unidos).
modo_preparacion(sanguinaria, cocimiento).
enfermedad(problemas_respiratorios).
enfermedad(dolor_garganta).
enfermedad(tos).
sintoma_enfermedad(problemas_respiratorios, dificultad_respirar).
sintoma_enfermedad(dolor_garganta, inflamacion_faringea).
sintoma_enfermedad(tos, irritacion_bronquial).
trata_enfermedad(sanguinaria, problemas_respiratorios).
trata_enfermedad(sanguinaria, dolor_garganta).
trata_enfermedad(sanguinaria, tos).
accion_efecto_planta(sanguinaria, expectorante).
accion_efecto_planta(sanguinaria, estimulante).
accion_efecto_planta(sanguinaria, antiseptica).
modo_tratamiento(sanguinaria, beber_o_hacer_gargaras).
precaucion_planta(sanguinaria, toxicidad_alta_si_se_abusa).

% Página 145 --- SENSITIVA ---
planta(sensativa).
nombre_cientifico(sensativa, 'Mimosa pudica').
continente_origen(sensativa, america).
pais_origen(sensativa, centroamerica).
modo_preparacion(sensativa, infusion_hojas).
enfermedad(ansiedad).
enfermedad(insomnio).
trata_enfermedad(sensativa, ansiedad).
trata_enfermedad(sensativa, insomnio).
accion_efecto_planta(sensativa, sedante).
accion_efecto_planta(sensativa, relajante).
modo_tratamiento(sensativa, noche).
precaucion_planta(sensativa, combinacion_sedantes).

% Página 146 --- SIMONILLO ---
planta(simonillo).
nombre_cientifico(simonillo, 'Tagetes lucida').
continente_origen(simonillo, america).
pais_origen(simonillo, mexico).
modo_preparacion(simonillo, te_flores).
enfermedad(problemas_estomacales).
enfermedad(parasitos_intestinales).
trata_enfermedad(simonillo, problemas_estomacales).
trata_enfermedad(simonillo, parasitos_intestinales).
accion_efecto_planta(simonillo, digestivo).
accion_efecto_planta(simonillo, antiparasitario).
modo_tratamiento(simonillo, en_ayunas).
precaucion_planta(simonillo, dosis_altas).

% Página 147 --- TAMARINDO ---
planta(tamarindo).
nombre_cientifico(tamarindo, 'Tamarindus indica').
continente_origen(tamarindo, africa).
pais_origen(tamarindo, sudan).
modo_preparacion(tamarindo, infusion_vainas).
modo_preparacion(tamarindo, pulpa).
enfermedad(estreñimiento).
enfermedad(fiebre).
trata_enfermedad(tamarindo, estreñimiento).
trata_enfermedad(tamarindo, fiebre).
accion_efecto_planta(tamarindo, laxante).
accion_efecto_planta(tamarindo, antipiretico).
modo_tratamiento(tamarindo, cuando_se_requiera).
precaucion_planta(tamarindo, diarrea).

% Página 148 --- TABACHIN ---
planta(tabachin).
nombre_cientifico(tabachin, 'Cesalpinia pulcherrima').
continente_origen(tabachin, america).
pais_origen(tabachin, mexico).
modo_preparacion(tabachin, infusion_flores).
enfermedad(tos).
enfermedad(bronquitis).
trata_enfermedad(tabachin, tos).
trata_enfermedad(tabachin, bronquitis).
accion_efecto_planta(tabachin, expectorante).
accion_efecto_planta(tabachin, antitusivo).
modo_tratamiento(tabachin, tres_veces_dia).
precaucion_planta(tabachin, embarazo).

% Página 149 --- TARAY ---
planta(taray).
nombre_cientifico(taray, 'Tamarix aphylla').
continente_origen(taray, asia).
pais_origen(taray, regiones_aridas).
modo_preparacion(taray, cocimiento_corteza).
enfermedad(problemas_renales).
enfermedad(inflamacion_ojos).
trata_enfermedad(taray, problemas_renales).
accion_efecto_planta(taray, diuretico).
accion_efecto_planta(taray, antiinflamatorio).
modo_tratamiento(taray, como_agua_de_uso).
precaucion_planta(taray, dosis_excesivas).

% Página 135 --- REGALIZ ---
planta(regaliz).
nombre_cientifico(regaliz, 'Glycyrrhiza glabra').
continente_origen(regaliz, europa).
modo_preparacion(regaliz, infusion).
enfermedad(tos).
enfermedad(dolor_garganta).
enfermedad(problemas_digestivos).
sintoma_enfermedad(tos, irritacion_garganta).
trata_enfermedad(regaliz, tos).
trata_enfermedad(regaliz, dolor_garganta).
trata_enfermedad(regaliz, problemas_digestivos).
accion_efecto_planta(regaliz, expectorante).
accion_efecto_planta(regaliz, digestivo).
modo_tratamiento(regaliz, pequenas_cantidades).
precaucion_planta(regaliz, hipertension).

% Página 136 --- RETAMA ---
planta(retama).
nombre_cientifico(retama, 'Spartium junceum').
continente_origen(retama, europa).
modo_preparacion(retama, cocimiento).
enfermedad(hipertension).
enfermedad(reumatismo).
sintoma_enfermedad(hipertension, presion_alta).
trata_enfermedad(retama, hipertension).
trata_enfermedad(retama, reumatismo).
accion_efecto_planta(retama, diuretico).
accion_efecto_planta(retama, hipotensor).
modo_tratamiento(retama, con_precaucion).
precaucion_planta(retama, toxicidad).

% Página 137 --- RICINO ---
planta(ricino).
nombre_cientifico(ricino, 'Ricinus communis').
continente_origen(ricino, africa).
modo_preparacion(ricino, maceracion).
enfermedad(estrenimiento).
enfermedad(inflamacion).
sintoma_enfermedad(estrenimiento, dificultad_evacuacion).
trata_enfermedad(ricino, estrenimiento).
trata_enfermedad(ricino, inflamacion).
accion_efecto_planta(ricino, laxante).
accion_efecto_planta(ricino, antiinflamatorio).
modo_tratamiento(ricino, uso_externo_o_ocasional).
precaucion_planta(ricino, toxicidad_semillas).

% Página 138 --- ROSAL ---
planta(rosal).
nombre_cientifico(rosal, 'Rosa centifolia').
continente_origen(rosal, asia).
modo_preparacion(rosal, infusion).
enfermedad(estres).
enfermedad(problemas_digestivos).
sintoma_enfermedad(estres, tension_nerviosa).
trata_enfermedad(rosal, estres).
trata_enfermedad(rosal, problemas_digestivos).
accion_efecto_planta(rosal, relajante).
accion_efecto_planta(rosal, digestivo).
modo_tratamiento(rosal, dos_tazas_al_dia).
precaucion_planta(rosal, alergia_cutanea).

% Página 139 --- ROMERO ---
planta(romero).
nombre_cientifico(romero, 'Rosmarinus officinalis').
continente_origen(romero, europa).
modo_preparacion(romero, infusion).
enfermedad(fatiga).
enfermedad(problemas_digestivos).
sintoma_enfermedad(fatiga, cansancio_general).
trata_enfermedad(romero, fatiga).
trata_enfermedad(romero, problemas_digestivos).
accion_efecto_planta(romero, estimulante).
accion_efecto_planta(romero, digestivo).
modo_tratamiento(romero, una_taza_manana).
precaucion_planta(romero, embarazo).

% Página 120 --- NOPAL ---
planta(nopal).
nombre_cientifico(nopal, 'Opuntia tuna').
continente_origen(nopal, asia).
pais_origen(nopal, japon).
modo_preparacion(nopal, pencas).
enfermedad(diabetes).
enfermedad(inflamacion_vejiga).
enfermedad(heridas).
enfermedad(hinchazones).
sintoma_enfermedad(inflamacion_vejiga, hinchazon).
trata_enfermedad(nopal, diabetes).
trata_enfermedad(nopal, inflamacion_vejiga).
trata_enfermedad(nopal, heridas).
trata_enfermedad(nopal, hinchazones).
accion_efecto_planta(nopal, laxante).
accion_efecto_planta(nopal, vermifugo).
modo_tratamiento(nopal, cataplasma).
modo_tratamiento(nopal, consumo_directo).

% Página 121 --- NOGAL ---
planta(nogal).
nombre_cientifico(nogal, 'Juglans regia').
continente_origen(nogal, europa).
pais_origen(nogal, iran).
modo_preparacion(nogal, hojas_y_corteza_en_te_o_infusion).
enfermedad(anemia).
enfermedad(herpes).
sintoma_enfermedad(anemia, fatiga).
sintoma_enfermedad(herpes, ampollas_cutaneas).
trata_enfermedad(nogal, anemia).
trata_enfermedad(nogal, escrofulosis).
trata_enfermedad(nogal, herpes).
trata_enfermedad(nogal, reumatismo).
accion_efecto_planta(nogal, tonico).
accion_efecto_planta(nogal, antireumatico).
accion_efecto_planta(nogal, nutritivo).
modo_tratamiento(nogal, te).
modo_tratamiento(nogal, infusion).
modo_tratamiento(nogal, consumo_directo).

% Página 122 --- NUEZ VOMICA ---
planta(nuez_vomica).
nombre_cientifico(nuez_vomica, 'Strychnos nux-vomica').
continente_origen(nuez_vomica, oceania).
utilizacion_historica(nuez_vomica, emetica).
utilizacion_historica(nuez_vomica, febrifuga).
alcaloide(nuez_vomica, estricnina).
alcaloide(nuez_vomica, brucina).
enfermedad(fiebres_malignas).
enfermedad(bronquitis).
enfermedad(reumas).
enfermedad(lombrices_intestinales).
enfermedad(tos_ferina).
trata_enfermedad(nuez_vomica, fiebres_malignas).
trata_enfermedad(nuez_vomica, bronquitis).
trata_enfermedad(nuez_vomica, reumas).
trata_enfermedad(nuez_vomica, lombrices_intestinales).
trata_enfermedad(nuez_vomica, tos_ferina).
modo_preparacion(nuez_vomica, te_espanol).
modo_tratamiento(nuez_vomica, te_espanol).
modo_tratamiento(nuez_vomica, hojas_te).

% Página 123 --- OCOTE ---
planta(ocote).
nombre_cientifico(ocote, 'Pinus teocote').
continente_origen(ocote, america).
pais_origen(ocote, mexico).
enfermedad(problemas_respiratorios).
enfermedad(dolor_muscular).
trata_enfermedad(ocote, problemas_respiratorios).
trata_enfermedad(ocote, dolor_muscular).
modo_preparacion(ocote, resina_mezclada).
modo_tratamiento(ocote, cataplasma).

% Página 124 --- ORTIGA ---
planta(ortiga).
nombre_cientifico(ortiga, 'Urtica urens').
continente_origen(ortiga, europa).
pais_origen(ortiga, 'regiones_templadas').
modo_preparacion(ortiga, hojas_frescas_o_secas).
modo_preparacion(ortiga, infusion).
modo_preparacion(ortiga, cataplasma).
enfermedad(anemia).
enfermedad(artritis).
enfermedad(reumatismo).
enfermedad(problemas_cutaneos).
enfermedad(caida_cabello).
sintoma_enfermedad(anemia, debilidad).
sintoma_enfermedad(artritis, dolor_articulaciones).
sintoma_enfermedad(problemas_cutaneos, irritacion_piel).
trata_enfermedad(ortiga, anemia).
trata_enfermedad(ortiga, artritis).
trata_enfermedad(ortiga, reumatismo).
trata_enfermedad(ortiga, problemas_cutaneos).
trata_enfermedad(ortiga, caida_cabello).
accion_efecto_planta(ortiga, diuretico).
accion_efecto_planta(ortiga, depurativo).
accion_efecto_planta(ortiga, antiinflamatorio).
modo_tratamiento(ortiga, te).
modo_tratamiento(ortiga, infusion).
modo_tratamiento(ortiga, cataplasma).
modo_tratamiento(ortiga, consumo_directo).

% Página 130 --- PRODIGIOSA ---
planta(prodigiosa).
nombre_cientifico(prodigiosa, 'Coleosanthus squarrosus').
continente_origen(prodigiosa, 'America').
pais_origen(prodigiosa, 'Mexico').
modo_preparacion(prodigiosa, 'Cocimiento').
enfermedad(disenteria).
enfermedad('cirrosis hepatica').
enfermedad(ictericia).
sintoma_enfermedad(disenteria, 'dolor abdominal').
sintoma_enfermedad('cirrosis hepatica', 'picazon en la piel').
sintoma_enfermedad(ictericia, 'piel amarillenta').
trata_enfermedad(prodigiosa, disenteria).
trata_enfermedad(prodigiosa, 'cirrosis hepatica').
trata_enfermedad(prodigiosa, ictericia).
accion_efecto_planta(prodigiosa, hepatoprotector).
accion_efecto_planta(prodigiosa, desintoxicante).
modo_tratamiento(prodigiosa, 'una taza en ayunas y otra antes de cada comida').
precaucion_planta(prodigiosa, embarazo).

% Página 131 --- PIRUL ---
planta(pirul).
nombre_cientifico(pirul, 'Schinus molle').
continente_origen(pirul, 'America').
pais_origen(pirul, 'Peru').
modo_preparacion(pirul, 'Maceracion').
enfermedad(gonorrea).
sintoma_enfermedad(gonorrea, 'ardor al orinar').
trata_enfermedad(pirul, gonorrea).
accion_efecto_planta(pirul, repelente).
accion_efecto_planta(pirul, purgante).
modo_tratamiento(pirul, '20 gotas cada 8 horas').
precaucion_planta(pirul, 'alergia polen').

% Página 132 --- PULSATILLA ---
planta(pulsatilla).
nombre_cientifico(pulsatilla, 'Anemone pulsatilla').
continente_origen(pulsatilla, 'Europa').
pais_origen(pulsatilla, 'Dinamarca').
modo_preparacion(pulsatilla, 'infusion').
enfermedad(herpes).
enfermedad('tos ferina').
enfermedad('enfermedades venereas').
enfermedad('jaquecas neuronales').
sintoma_enfermedad(herpes, llagas).
sintoma_enfermedad('tos ferina', 'secreción nasal').
sintoma_enfermedad('enfermedades venereas', 'llagas').
sintoma_enfermedad('jaquecas neuronales', 'nauseas').
trata_enfermedad(pulsatilla, herpes).
trata_enfermedad(pulsatilla, 'tos ferina').
trata_enfermedad(pulsatilla, 'enfermedades venereas').
trata_enfermedad(pulsatilla, 'jaquecas neuronales').
accion_efecto_planta(pulsatilla, calmante).
accion_efecto_planta(pulsatilla, antiespasmodico).
modo_tratamiento(pulsatilla, 'tres a cinco veces al dia').
precaucion_planta(pulsatilla, 'irritante severo en piel').

% Página 133 --- QUEBRACHO ---
planta(quebracho).
nombre_cientifico(quebracho, 'Lysiloma auritum').
continente_origen(quebracho, 'America').
pais_origen(quebracho, 'Chile').
modo_preparacion(quebracho, 'infusion').
enfermedad('inflamaciones intestinales').
enfermedad('flujo').
enfermedad('afecciones del rinon').
sintoma_enfermedad('inflamaciones intestinales', 'cansancio').
sintoma_enfermedad('flujo', 'entumecimiento').
sintoma_enfermedad('afecciones del rinon', 'dolor en espalda').
trata_enfermedad(quebracho, 'inflamaciones intestinales').
trata_enfermedad(quebracho, 'flujo').
trata_enfermedad(quebracho, 'afecciones del rinon').
accion_efecto_planta(quebracho, antiasmatico).
accion_efecto_planta(quebracho, descongestivo).
modo_tratamiento(quebracho, 'una a tres tazas al dia').
precaucion_planta(quebracho, lactancia).

% Página 134 --- QUINA ---
planta(quina).
nombre_cientifico(quina, 'Cinchona calisaya').
continente_origen(quina, 'America').
pais_origen(quina, 'Bolivia').
modo_preparacion(quina, 'infusion').
enfermedad('tos ferina').
enfermedad('asma').
enfermedad(tetano).
enfermedad(epilepsia).
enfermedad(eclampsia).
sintoma_enfermedad('tos ferina', 'secreción nasal').
sintoma_enfermedad('asma', 'sibilancias').
sintoma_enfermedad(tetano, 'espasmos').
sintoma_enfermedad(epilepsia, 'crisis epileptica').
sintoma_enfermedad(eclampsia, 'convulsiones').
trata_enfermedad(quina, 'tos ferina').
trata_enfermedad(quina, 'asma').
trata_enfermedad(quina, tetano).
trata_enfermedad(quina, epilepsia).
trata_enfermedad(quina, eclampsia).
accion_efecto_planta(quina, antiasmatico).
accion_efecto_planta(quina, antimicrobiano).
modo_tratamiento(quina, 'una a dos tazas al dia').
precaucion_planta(quina, alergia).

% Página 150 --- TILA ---
planta(tila).
nombre_cientifico(tila, 'Tilia mexicana').
continente_origen(tila, america).
pais_origen(tila, mexico).
modo_preparacion(tila, infusion).
enfermedad(insomnio).
enfermedad(ansiedad).
enfermedad(hipertension).
sintoma_enfermedad(insomnio, dificultad_para_dormir).
trata_enfermedad(tila, insomnio).
trata_enfermedad(tila, ansiedad).
accion_efecto_planta(tila, calmante).
accion_efecto_planta(tila, sedante).
modo_tratamiento(tila, una_vez_noche).
precaucion_planta(tila, hipersensibilidad).

% Página 151 --- TÉ DE MILPA ---
planta(te_de_milpa).
nombre_cientifico(te_de_milpa, 'Zea mays').
continente_origen(te_de_milpa, america).
pais_origen(te_de_milpa, mexico).
modo_preparacion(te_de_milpa, cocimiento).
enfermedad(infeccion_urinaria).
enfermedad(retencion_liquidos).
enfermedad(calculos_renales).
sintoma_enfermedad(infeccion_urinaria, dolor_al_orinar).
trata_enfermedad(te_de_milpa, infeccion_urinaria).
trata_enfermedad(te_de_milpa, retencion_liquidos).
accion_efecto_planta(te_de_milpa, diuretico).
accion_efecto_planta(te_de_milpa, depurativo).
modo_tratamiento(te_de_milpa, dos_veces_dia).
precaucion_planta(te_de_milpa, alergia_maiz).

% Página 152 --- TOLOACHE ---
planta(toloache).
nombre_cientifico(toloache, 'Datura stramonium').
continente_origen(toloache, america).
pais_origen(toloache, mexico).
modo_preparacion(toloache, infusion).
enfermedad(dolor_muscular).
enfermedad(asma).
enfermedad(insomnio).
sintoma_enfermedad(dolor_muscular, rigidez_muscular).
trata_enfermedad(toloache, dolor_muscular).
trata_enfermedad(toloache, asma).
accion_efecto_planta(toloache, analgesico).
accion_efecto_planta(toloache, broncodilatador).
modo_tratamiento(toloache, uso_externo_o_dosis_baja).
precaucion_planta(toloache, toxicidad_alta).

% Página 153 --- TRONADORA ---
planta(tronadora).
nombre_cientifico(tronadora, 'Tecoma stans').
continente_origen(tronadora, america).
pais_origen(tronadora, mexico).
modo_preparacion(tronadora, infusion).
enfermedad(diabetes).
enfermedad(fiebre).
enfermedad(problemas_digestivos).
sintoma_enfermedad(diabetes, glucosa_alta).
trata_enfermedad(tronadora, diabetes).
trata_enfermedad(tronadora, fiebre).
accion_efecto_planta(tronadora, hipoglucemiante).
accion_efecto_planta(tronadora, antipiretico).
modo_tratamiento(tronadora, dos_veces_dia).
precaucion_planta(tronadora, hipoglucemia).

% Página 154 --- TRIPA DE JUDAS ---
planta(tripa_de_judas).
nombre_cientifico(tripa_de_judas, 'Leonotis nepetifolia').
continente_origen(tripa_de_judas, america).
pais_origen(tripa_de_judas, mexico).
modo_preparacion(tripa_de_judas, cocimiento).
enfermedad(tos).
enfermedad(gripe).
enfermedad(dolor_estomacal).
sintoma_enfermedad(tos, irritacion_garganta).
trata_enfermedad(tripa_de_judas, tos).
trata_enfermedad(tripa_de_judas, gripe).
accion_efecto_planta(tripa_de_judas, expectorante).
accion_efecto_planta(tripa_de_judas, antiespasmodico).
modo_tratamiento(tripa_de_judas, tres_veces_dia).
precaucion_planta(tripa_de_judas, dosis_excesiva).

% Página 155 --- UVA ---
planta(uva).
nombre_cientifico(uva, 'Vitis vinifera').
continente_origen(uva, europa).
pais_origen(uva, espana).
modo_preparacion(uva, jugo_o_consumo_directo).
enfermedad(estreñimiento).
enfermedad(hipertension).
enfermedad(anemia).
sintoma_enfermedad(estreñimiento, dificultad_evacuar).
trata_enfermedad(uva, estreñimiento).
trata_enfermedad(uva, hipertension).
accion_efecto_planta(uva, antioxidante).
accion_efecto_planta(uva, laxante).
modo_tratamiento(uva, consumo_diario).
precaucion_planta(uva, diabetes).

% ---------------------------------- % REGLAS % ----------------------------------

plantas_medicinales(Plantas) :- 
    findall(Planta, planta(Planta), Plantas).

nombre_cientifico_planta(Planta, Nombre) :- 
    nombre_cientifico(Planta, Nombre).

enfermedades_que_trata(Planta, Enfermedades) :- 
    findall(Enfermedad, trata(Planta, Enfermedad), Enfermedades).

plantas_para_enfermedad(Enfermedad, Plantas) :- 
    findall(Planta, trata(Planta, Enfermedad), Plantas).

modos_preparacion(Planta, Modos) :- 
    findall(Modo, modo_preparacion(Planta, Modo), Modos).

origen_planta(Planta, Continente, Pais) :- 
    origen(Planta, Continente, Pais).

acciones_planta(Planta, Acciones) :- 
    findall(Accion, accion(Planta, Accion), Acciones).

precauciones_planta(Planta, Precauciones) :- 
    findall(Precaucion, precaucion(Planta, Precaucion), Precauciones).

plantas_con_accion(Accion, Plantas) :- 
    findall(Planta, accion(Planta, Accion), Plantas).

sintomas_enfermedad(Enfermedad, Sintomas) :- 
    findall(Sintoma, sintoma(Enfermedad, Sintoma), Sintomas).

tratamientos_enfermedad(Enfermedad, Tratamientos) :- 
    findall((Planta, Preparacion), 
           (trata(Planta, Enfermedad), modo_preparacion(Planta, Preparacion)), 
           Tratamientos).

info_planta(Planta) :- 
    planta(Planta), 
    nombre_cientifico(Planta, Cientifico), 
    (origen(Planta, Continente, Pais) -> true ; Continente = desconocido, Pais = desconocido), 
    findall(Enf, trata(Planta, Enf), Enfermedades), 
    findall(Modo, modo_preparacion(Planta, Modo), Modos), 
    findall(Acc, accion(Planta, Acc), Acciones), 
    findall(Prec, precaucion(Planta, Prec), Precauciones),

    format('=== INFORMACION DE PLANTA ===~n', []),
    format('Nombre comun: ~w~n', [Planta]),
    format('Nombre cientifico: ~w~n', [Cientifico]),
    format('Origen: ~w/~w~n~n', [Continente, Pais]),

    format('Enfermedades que trata:~n', []),
    (   Enfermedades = [] -> format('  No hay informacion registrada~n', [])
    ;   forall(member(Enf, Enfermedades), format('  - ~w~n', [Enf]))),

    format('~nModos de preparacion:~n', []),
    (   Modos = [] -> format('  No hay informacion registrada~n', [])
    ;   forall(member(Modo, Modos), format('  - ~w~n', [Modo]))),

    format('~nAcciones farmacologicas:~n', []),
    (   Acciones = [] -> format('  No hay informacion registrada~n', [])
    ;   forall(member(Acc, Acciones), format('  - ~w~n', [Acc]))),

    format('~nPrecauciones:~n', []),
    (   Precauciones = [] -> format('  No hay precauciones registradas~n', [])
    ;   forall(member(Prec, Precauciones), format('  - ~w~n', [Prec]))),
    nl.

% ---------------------------------- % FUNCIONES % ----------------------------------

% Obtener la ruta absoluta de la imagen
obtener_imagen_planta(Planta, RutaImagen) :-
    % Obtener el directorio de trabajo actual
    working_directory(DirActual, DirActual),
    % Construir la ruta de la imagen específica
    format(atom(RutaEspecifica), '~w/imgs/~w.jpg', [DirActual, Planta]),
    % Verificar si el archivo existe
    (exists_file(RutaEspecifica) ->
        RutaImagen = RutaEspecifica
    ;
        % Si no existe, usar la imagen por defecto
        format(atom(RutaDefecto), '~w/imgs/default.jpg', [DirActual]),
        RutaImagen = RutaDefecto
    ).

% Verificar si un archivo existe
exists_file(File) :-
    catch(
        (open(File, read, Stream),
         close(Stream)),
        _,
        fail
    ).

% Mostrar resultados en una ventana
mostrar_en_ventana(Titulo, Contenido) :-
    new(D, dialog(Titulo)),
    send(D, size, size(600, 400)),

    new(Box, device),
    send(Box, size, size(580, 350)),
    
    new(V, view),
    send(V, size, size(580, 350)),
    send(V, editable, @off),
    send(V, font, font(courier, roman, 12)),
    send(V, contents, Contenido),

    send(D, append, V),
    send(D, append, button(cerrar, message(D, destroy))),
    send(D, open).

% Mostrar información
mostrar_info_con_imagen(Titulo, Contenido, RutaImagen) :-
    new(D, dialog(Titulo)),
    send(D, size, size(700, 550)),

    new(FrameHorizontal, dialog_group('')),
    send(D, append, FrameHorizontal),

    catch(
        (% Crear la imagen como un picture para mejor control
         new(Picture, picture),
         send(Picture, size, size(200, 200)),
         new(Imagen, bitmap(RutaImagen)),
         send(Picture, display, Imagen),
         send(FrameHorizontal, append, Picture)
        ),
        _Error,
        (% Si hay error cargando la imagen, mostrar mensaje
         new(MensajeImagen, label(imagen, 'Imagen no disponible')),
         send(MensajeImagen, font, font(helvetica, italic, 12)),
         send(FrameHorizontal, append, MensajeImagen)
        )
    ),

    new(Editor, editor),
    send(Editor, size, size(450, 380)),
    send(Editor, editable, @off),
    send(Editor, font, font(courier, roman, 11)),
    send(Editor, append, Contenido),
    send(FrameHorizontal, append, Editor),
    
    send(D, append, button(cerrar, message(D, destroy))),
    send(D, open).

% Capturar salida 
capture_output(Goal, Output) :-
    with_output_to(string(Output), Goal).

% ---------------------------------- %CONSULTAS% ----------------------------------

% Buscar si una planta
buscar_planta(Nombre) :-
    (planta(Nombre) ->
        with_output_to(string(Mensaje), 
                      format('¡La planta "~w" existe en la base de datos!', [Nombre])),
        mostrar_en_ventana('Planta encontrada', Mensaje)
    ;
        with_output_to(string(Mensaje), 
                      format('La planta "~w" no se encuentra en la base de datos.', [Nombre])),
        mostrar_en_ventana('Planta no encontrada', Mensaje)
    ).

% Mostrar información
mostrar_info_planta_ventana(Nombre) :-
    (planta(Nombre) ->
        capture_output(info_planta(Nombre), Resultado),
        obtener_imagen_planta(Nombre, RutaImagen),
        with_output_to(string(Titulo), format('Informacion de ~w', [Nombre])),
        mostrar_info_con_imagen(Titulo, Resultado, RutaImagen)
    ;
        with_output_to(string(Mensaje), 
                      format('La planta "~w" no se encuentra en la base de datos.', [Nombre])),
        mostrar_en_ventana('Error', Mensaje)
    ).

% Listar todas las plantas
listar_plantas_ventana :-
    plantas_medicinales(Plantas),
    with_output_to(string(Resultado),
                   (format('Plantas medicinales disponibles:~n~n', []),
                    forall(member(P, Plantas), (format('- ~w~n', [P]))))),
    mostrar_en_ventana('Lista de Plantas', Resultado).

% Mostrar enfermedades que trata
enfermedades_por_planta_ventana :-
    capture_output(
        (format('Plantas y enfermedades que tratan:~n~n', []),
         forall(planta(P), 
                (enfermedades_que_trata(P, Enfermedades), 
                 format('~w trata: ~w~n~n', [P, Enfermedades])))),
        Resultado),
    mostrar_en_ventana('Enfermedades por Planta', Resultado).

% Mostrar plantas para cada enfermedad
plantas_por_enfermedad_ventana :-
    capture_output(
        (format('Enfermedades y plantas que las tratan:~n~n', []),
         forall(enfermedad(E), 
                (plantas_para_enfermedad(E, Plantas), 
                 format('~w puede tratarse con: ~w~n~n', [E, Plantas])))),
        Resultado),
    mostrar_en_ventana('Plantas por Enfermedad', Resultado).

% Mostrar plantas para una enfermedad
plantas_para_enfermedad_ventana(Enfermedad) :-
    atom_string(EnfermedadAtom, Enfermedad),
    (enfermedad(EnfermedadAtom) ->
        plantas_para_enfermedad(EnfermedadAtom, Plantas),
        with_output_to(string(Resultado),
                      (format('Plantas que tratan ~w:~n~n', [Enfermedad]),
                       forall(member(P, Plantas), format('- ~w~n', [P])))),
        mostrar_en_ventana('Plantas para enfermedad', Resultado)
    ;
        with_output_to(string(Mensaje), 
                      format('La enfermedad "~w" no se encuentra en la base de datos.', [Enfermedad])),
        mostrar_en_ventana('Error', Mensaje)
    ).

% Mostrar plantas con una acción
plantas_con_accion_ventana(Accion) :-
    atom_string(AccionAtom, Accion),
    plantas_con_accion(AccionAtom, Plantas),
    with_output_to(string(Resultado),
                  (format('Plantas con accion ~w:~n~n', [Accion]),
                   (Plantas = [] -> 
                       format('No se encontraron plantas con esta accion.~n', [])
                   ;
                       forall(member(P, Plantas), format('- ~w~n', [P]))))),
    mostrar_en_ventana('Plantas por accion', Resultado).

% Mostrar modos de preparación por planta
preparaciones_ventana :-
    capture_output(
        (format('Modos de preparacion por planta:~n~n', []),
         forall(planta(P), 
                (modos_preparacion(P, Modos), 
                 format('~w: ~w~n~n', [P, Modos])))),
        Resultado),
    mostrar_en_ventana('Modos de Preparacion', Resultado).

% Mostrar acciones farmacológicas por planta
acciones_ventana :-
    capture_output(
        (format('Acciones farmacologicas por planta:~n~n', []),
         forall(planta(P), 
                (acciones_planta(P, Acciones), 
                 format('~w: ~w~n~n', [P, Acciones])))),
        Resultado),
    mostrar_en_ventana('Acciones Farmacologicas', Resultado).

% Mostrar precauciones por planta
precauciones_ventana :-
    capture_output(
        (format('Precauciones por planta:~n~n', []),
         forall(planta(P), 
                (precauciones_planta(P, Precauciones), 
                 format('~w: ~w~n~n', [P, Precauciones])))),
        Resultado),
    mostrar_en_ventana('Precauciones', Resultado).

% Mostrar plantas por modo de preparación
plantas_por_modo_ventana(Modo) :-
    atom_string(ModoAtom, Modo),
    findall(P, modo_preparacion(P, ModoAtom), Plantas),
    with_output_to(string(Resultado),
                  (format('Plantas con modo de preparacion ~w:~n~n', [Modo]),
                   (Plantas = [] -> 
                       format('No se encontraron plantas con este modo de preparacion.~n', [])
                   ;
                       forall(member(P, Plantas), format('- ~w~n', [P]))))),
    mostrar_en_ventana('Plantas por modo de preparacion', Resultado).

% Mostrar plantas por precaución
plantas_por_precaucion_ventana(Precaucion) :-
    atom_string(PrecaucionAtom, Precaucion),
    findall(P, precaucion(P, PrecaucionAtom), Plantas),
    with_output_to(string(Resultado),
                  (format('Plantas con precaucion ~w:~n~n', [Precaucion]),
                   (Plantas = [] -> 
                       format('No se encontraron plantas con esta precaucion.~n', [])
                   ;
                       forall(member(P, Plantas), format('- ~w~n', [P]))))),
    mostrar_en_ventana('Plantas por precaucion', Resultado).

% ---------------------------------- % INTERFAZ % ----------------------------------

% Ventana principal 
interfaz :- 
    new(Ventana, dialog('Yerberito')),
    send(Ventana, size, size(750, 500)),
    
    % Título
    send(Ventana, append, 
         new(TituloLabel, label(nombre, 'Yerberito'))),
    send(TituloLabel, font, font(helvetica, bold, 18)),
    send(TituloLabel, colour, '#40661a'),
    
    % Campo de búsqueda
    new(PlantaInput, text_item('Nombre de la planta:')),
    send(Ventana, append, PlantaInput),
    
    % Grupo para botones de consulta
    new(BotonesConsulta, dialog_group('Consultas de Plantas')),
    send(Ventana, append, BotonesConsulta),
    
    send(BotonesConsulta, append, button('Buscar planta', 
                                         message(@prolog, buscar_planta, PlantaInput?selection))),
    send(BotonesConsulta, append, button('Informacion completa', 
                                         message(@prolog, mostrar_info_planta_ventana, PlantaInput?selection))),
    send(BotonesConsulta, append, button('Lista', 
                                         message(@prolog, listar_plantas_ventana))),
    
    % Grupo para botones de consultas generales
    new(BotonesGenerales, dialog_group('Consultas Generales')),
    send(Ventana, append, BotonesGenerales),
    
    send(BotonesGenerales, append, button('Enfermedades por planta', 
                                          message(@prolog, enfermedades_por_planta_ventana))),
    send(BotonesGenerales, append, button('Plantas por enfermedad', 
                                          message(@prolog, plantas_por_enfermedad_ventana))),
    send(BotonesGenerales, append, button('Preparaciones', 
                                          message(@prolog, preparaciones_ventana))),
    send(BotonesGenerales, append, button('Farmacologicas', 
                                          message(@prolog, acciones_ventana))),
    send(BotonesGenerales, append, button('Precauciones', 
                                          message(@prolog, precauciones_ventana))),
    
    % Grupo para búsquedas específicas
    new(BusquedasEspecificas, dialog_group('Busquedas Especificas')),
    send(Ventana, append, BusquedasEspecificas),
    
    new(EnfermedadInput, text_item('Enfermedad:')),
    send(BusquedasEspecificas, append, EnfermedadInput),
    send(BusquedasEspecificas, append, button('Buscar', 
                                              message(@prolog, plantas_para_enfermedad_ventana, EnfermedadInput?selection))),
    
    new(AccionInput, text_item('Accion farmacologica:')),
    send(BusquedasEspecificas, append, AccionInput),
    send(BusquedasEspecificas, append, button('Buscar', 
                                              message(@prolog, plantas_con_accion_ventana, AccionInput?selection))),
                                              
    new(ModoInput, text_item('Modo de preparacion:')),
    send(BusquedasEspecificas, append, ModoInput),
    send(BusquedasEspecificas, append, button('Buscar', 
                                              message(@prolog, plantas_por_modo_ventana, ModoInput?selection))),
                                              
    new(PrecaucionInput, text_item('Precaucion:')),
    send(BusquedasEspecificas, append, PrecaucionInput),
    send(BusquedasEspecificas, append, button('Buscar', 
                                              message(@prolog, plantas_por_precaucion_ventana, PrecaucionInput?selection))),
    
    % Botón de salida
    send(Ventana, append, button('Salir', message(Ventana, destroy))),
    
    send(Ventana, open).

:- initialization(interfaz).