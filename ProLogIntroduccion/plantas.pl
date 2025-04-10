% SIGNIFICADO DE ALGUNAS PALABRAS
significado(afrodisiaca,excita).
significado(analgesica,modera_dolor).
significado(anestesica,insensibiliza_cuerpo).
significado(antidiarreica,controla_deposiciones).
significado(antiespasmodica,controla_espasmo).
significado(antiflogistica,inflamacion).
significado(antipiretica,disminuye_fiebre).
significado(antiseptica,mata_tejidos).
significado(aperitiva,apetito).
significado(astringente,contrae_tejidos).
significado(carminativa,gases).
significado(colagoga,bilis).
significado(depurativa,purifica_sangre).
significado(diaforetica,sudoracion).
significado(digestiva,digestion).
significado(diuretica,orina).
significado(emetica,vomito).
significado(emenagoga,menstruacion).
significado(estupefaciente,duerme).
significado(expectorante,flemas).
significado(hemostatica,hemorragias).
significado(hepatica,higado).
significado(laxante,purga).
significado(pectoral,pecho).
significado(sedante,intestino).
significado(tonica,organismo_fuerte).
significado(toxica,venenosa).
significado(vermifuga,gusanos_intestinales).
significado(vulneraria,heridas).
significado(antibacteriana,elimina_bacterias).
significado(antiviral,elimina_virus).
significado(antifungica,elimina_hongos).
significado(antiparasitaria,elimina_parasitos).
significado(antioxidante,previene_envejecimiento).

%HECHOS
% Plantas
planta(oregano).
planta(palo_de_flor).
planta(pasiflora).
planta(pericon).
planta(pinguica).

% Qué enfermedades curan
cura(oregano, colicos).
cura(palo_de_flor, infecciones).
cura(pasiflora, insomnio).
cura(pericon, estres).
cura(pinguica, infecciones_urinarias).

% Sistemas que fortalecen
fortalece(oregano, sistema_digestivo).
fortalece(pasiflora, sistema_nervioso).
fortalece(pericon, sistema_nervioso).
fortalece(pinguica, vias_urinarias).

% Formas de uso
forma_uso(oregano, infusion).
forma_uso(palo_de_flor, decoccion).
forma_uso(pasiflora, infusion).
forma_uso(pericon, infusion).
forma_uso(pinguica, cocimiento).

relacionado(sistema_digestivo, colicos).
relacionado(sistema_nervioso, insomnio).
relacionado(sistema_nervioso, estres).
relacionado(vias_urinarias, infecciones_urinarias).
relacionado(sistema_inmunologico, infecciones).

%REGLAS

% Una planta es buena para una enfermedad
buena_para(Planta, Enfermedad) :- cura(Planta, Enfermedad).

buena_para(Planta, Enfermedad) :- fortalece(Planta, Sistema), relacionado(Sistema, Enfermedad).

% Una planta sirve
sirve_para_sistema(Planta, Sistema) :- fortalece(Planta, Sistema).

sirve_para_sistema(Planta, Sistema) :- cura(Planta, Enfermedad), relacionado(Sistema, Enfermedad).

% Saber qué formas de uso sirven para una enfermedad
uso_para(Enfermedad, Forma) :- buena_para(Planta, Enfermedad), forma_uso(Planta, Forma).

% Obtener una lista de plantas que ayudan con una enfermedad
plantas_para(Enfermedad, Lista) :- findall(Planta, buena_para(Planta, Enfermedad), Lista).