-- a.	RochaMcQueen que tiene 300 litros, su velocidad inicial es 0,  su enamorado es Ronco y su truco es deReversa.

-- b.	Biankker (nuestro tanque ruso) tiene 500 litros, su velocidad inicial es 20, su enamorado es Tinch y su truco es impresionar.
				
-- c.	Gushtav tiene 200 litros, su velocidad inicial es 130, su enamorada es PetiLaLinda y su truco es nitro

-- d.	Rodra que se olvidó de cargar nafta (tiene 0 litros), su velocidad inicial es 50, su enamorada es Taisa y su truco es fingirAmor con Petra.

import Text.Show.Functions


data Auto = Auto {	nombre :: String,
					litros :: Int,
					velocidad :: Int,
					enamorado :: String,
					truco :: Truco
} deriving (Show)

type Truco = Auto -> Auto

rochaMcQueen = Auto "RochaMcQueen" 300 0 "Ronco" deReversa
biankker = Auto "Biankker" 500 20 "Tinch" impresionar
gushtav = Auto "Gushtav" 200 130 "PetiLaLinda" nitro
rodra = Auto "Rodra" 0 50 "Taisa" (fingirAmor "Petra")



modificarVelocidad :: (Int -> Int) -> Truco
modificarVelocidad funcion auto = auto {velocidad = (funcion . velocidad) auto }

--deReversa que hace que suba la nafta en un quinto de la cantidad de metros de la pista en donde está corriendo.
-- Por el momento, supondremos que la pista tendrá 1000 metros.
deReversa :: Truco
deReversa auto = auto {litros = ((+(quot (velocidad auto) 5)).litros) auto}

--	impresionar duplica su velocidad para impresionar a su enamorade.
impresionar :: Truco
impresionar = modificarVelocidad (*2)

--	nitro aumenta su velocidad en 15 km/h instantáneamente.
nitro :: Truco
nitro auto = auto {velocidad = ((+15) . velocidad) auto}

--	fingirAmor elije a otre enamorade por conveniencia (veremos para qué sirve más adelante). 
fingirAmor :: String -> Truco
fingirAmor nuevoEnamorado auto = auto {enamorado = nuevoEnamorado}

-- incrementarVelocidad según la cantidad de vocales de le enamorade de su auto:
--Si tiene entre 1 y 2 letras aumenta 15 km/h.
--Si tiene entre 3 y 4 aumenta 20 km/h.
--Si tiene más de 4 aumenta 30 km/h.

esVocal :: Char -> Bool
esVocal caracter = elem caracter "aeiouAEIOU" -- ['a','e','i','o','u','A','E','I','O','U']

cantVocales :: String -> Int
cantVocales = length . filter esVocal

cantVocalesEnamorado :: Auto -> Int
cantVocalesEnamorado = (cantVocales . enamorado)

-- Pasar la cantidad de vocales en incrementar velocidad
extraVelocidadEnamorado :: Int -> Int
extraVelocidadEnamorado cantVocales
 |cantVocales == 0 = 0
 |cantVocales <= 2 = 15
 |cantVocales <= 4 = 20
 |otherwise = 30

incrementarVelocidad :: Truco
incrementarVelocidad auto = modificarVelocidad +((extraVelocidadEnamorado.cantVocalesEnamorado) auto) auto

puedeRealizarTruco :: Auto -> Bool
puedeRealizarTruco auto = ((>0) . litros) auto && ((<100) . velocidad) auto

comboLoco :: Truco
comboLoco = nitro . deReversa

queTrucazo :: String -> Truco
queTrucazo nuevoEnamorado = incrementarVelocidad . fingirAmor nuevoEnamorado

vaciarTanque :: Truco
vaciarTanque auto = auto {litros = 0} 

turbo :: Truco
turbo auto = (vaciarTanque . (modificarVelocidad (+((*10) . litros) auto))) auto



--De las carreras conocemos la cantidad de vueltas que se deben correr, la longitud de la pista en kilómetros, los nombres de los integrantes del público, una trampa y los participantes, claro.

data Carrera = Carrera {	cantVueltas :: Int,
							longitudPista :: Int,
							integrantesPublico :: [String],
							trampa :: Trampa,
							participantes :: [Auto]
} deriving (Show)

type Trampa = Carrera -> Carrera
modificarParticipantes :: ([Auto] -> [Auto]) -> Trampa
modificarParticipantes funcion carrera = carrera { participantes = funcion (participantes carrera) }

--sacarAlPistero: el primer participante queda fuera de la competencia.
sacarAlPistero :: Trampa 
sacarAlPistero carrera = modificarParticipantes tail carrera

--Modelar la carrera potreroFunes en la que el largo de la pista es de 5,0 kilómetros, se deben dar 3 vueltas, en el público están Ronco, Tinch, y Dodain, y en la que participan RochaMcQueen, Biankerr, Gushtav y Rodra (quienes salen de la línea de meta en ese mismo orden). Su trampa es sacarAlPistero. 
potreroFunes = Carrera 3 5 ["Ronco","Tincho","Dodain"] sacarAlPistero [rochaMcQueen,biankker,gushtav,rodra]

modificarVelocidadDeParticipantes :: (Int -> Int) -> Carrera -> Carrera
modificarVelocidadDeParticipantes funcion carrera = modificarParticipantes (map f (participantes carrera)) carrera
 where f = modificarVelocidad funcion

--lluvia: baja 10 km/h de velocidad a todos los participantes.
lluvia :: Trampa
lluvia carrera = modificarParticipantes (map f (participantes carrera)) carrera
 where f = modificarVelocidad -10

inutilidad :: Truco
inutilidad auto = auto {truco = inutilidad}

--neutralizarTrucos: inutiliza a todos los participantes. En otras palabras, todos tendrán el truco inutilidad, que no hace nada.
neutralizarTrucos :: [Auto] -> [Auto]
neutralizarTrucos = map inutilidad

--pocaReserva: los participantes con menos de 30 litros de nafta quedan fuera.
pocaReserva :: Trampa
pocaReserva carrera = modificarParticipantes (filter (litros >= 30)) carrera

--podio: solamente deja seguir compitiendo a los pºrimeros 3 participantes de la carrera.
--podio :: Trampa
--podio carrera = modificarParticipantes recursividad??

naftaVueltaAuto :: Int -> Truco
naftaVueltaAuto kmPista auto = auto {litros = litros - (velocidad * (quot kmPista 10))}

naftaVueltaTodos :: Trampa
naftaVueltaTodos carrera = modificarParticipantes (map f) carrera --  carrera {participantes = map (naftaVueltaAuto longitudPista) participantes}
 where f = naftaVueltaAuto (longitudPista carrera)

chequearStringEnLista :: String -> [String] -> Bool
chequearStringEnLista palabra lista = length (filter (==palabra) lista) > 0

llamarLaAtencionEnamorade :: [String] -> Truco
llamarLaAtencionEnamorade publico auto
 |chequearStringEnLista (enamorado auto) publico = (truco auto) auto
 |otherwise = auto

llamarLaAtencionEnamorades :: Trampa
llamarLaAtencionEnamorades carrera = modificarParticipantes (map f) carrera 
 where f = llamarLaAtencionEnamorade (integrantesPublico carrera)

aplicarTrampa :: Trampa
aplicarTrampa carrera = (trampa carrera) carrera

darVuelta :: Trampa
darVuelta carrera =  (aplicarTrampa . llamarLaAtencionEnamorades . naftaVueltaTodos) carrera

-- FIN

correrCarrera :: Trampa
correrCarrera carrera = (iterate darVuelta carrera) !! cantVueltas carrera

--quienGana :: Carrera -> Auto
--quienGana carrera = 

--ESTA ES IGUAL QUE PODIO (MISMA DUDA, SI USAR O NO RECURSIVIDAD, EN CASO DE QUE NO, COMO SE HACE) see
--quienGana :: Carrera -> Auto
--quienGana carrera = --comparacion velocidades autos


--ESTE SE RECURSIVO FIJA, APLICA UNO POR UNO CADA TRUCO COMIENDO LA CABEZA Y 
--DEJANDO LA COLA PARA Q VUELVA A AGARRAR LA CABEZA, ETC. PERO LE PREGUNTAMOS TMB POR LAS DUDAS
--elGranTruco :: [Truco] -> Auto
--elGranTruco trucos, auto = --recursivdad..

{-
3.6
a) No.
b) Si.
c) No.
-}




