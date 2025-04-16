{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Library where
import PdePreludat
import Foreign.Safe (Int)

doble :: Number -> Number
doble numero = numero + numero

saludo :: String -> String
saludo nombre = "Hola, " ++ nombre ++ "! Bienvenida a Haskell ✨"

miFuncion :: Number -> Bool
miFuncion 2 = True

--Funcional 1: Primeros ejercicios

--punto 1 devuelve true si un numero es multiplo de 3
esMultiploDeTres :: Number -> Bool
esMultiploDeTres numero = numero `mod` 3 == 0   --tmb se puede escribir esMultiploDeTres = (== 0) . (`mod` 3) 

--punto 2 devuelve ture si el segundo es multiplo del primero ( ej ingresa 12 y 3 y es true pq 12 es multiplo de 3)
esMultiploDe :: Number -> Number -> Bool
esMultiploDe numero1 numero2 = numero2 `mod` numero1 == 0  --tmb se puede escribir esMultiploDe numero1 = (==0) . (`mod` numero1) 

--punto 3 devuelve el cubo de un numero
cubo :: Number -> Number
cubo numero = numero * numero * numero 

--punto 4 devuelve el area de un rectangulo
area :: Number -> Number -> Number
area base altura = base * altura

--punto 5 indica si un año es bisiesto
esBisiesto :: Number -> Bool
esBisiesto numero = esMultiploDe 400 numero || esMultiploDe 4 numero && not (esMultiploDe 100 numero)

--punto 6 pasa una temp de Celsius a Fahrenheit

celsiusToFahr :: Number -> Number 
celsiusToFahr celsius = celsius * 1.8 + 32

--punto 7 pasa una temp de Fahrenheit a Celsius

fahrToCelsius :: Number -> Number
fahrToCelsius fahr = (fahr - 32) / 1.8

--punto 8 indica True si la temp en fahr es menor a 8 grados en Celsius

haceFrioF :: Number -> Bool
haceFrioF fahr= fahrToCelsius fahr < 8   

--punto 9 devuelve el minimo comun multiplo. El gcd lit hace eso

mcm :: Number -> Number -> Number 
mcm numero1 numero2 = numero1 * numero2 / gcd numero1 numero2

--punto 10: ya empiezan los problemas
--a

--Devuelve el mayor de 3 numeros
maxDe3 :: Number -> Number -> Number -> Number
maxDe3 numero1 numero2 numero3 = max numero1 (max numero2 numero3)

--Devuelve el menor de 3 numeros
minDe3 :: Number -> Number -> Number -> Number
minDe3 numero1 numero2 numero3 = min numero1 (min numero2 numero3)

--Devuelve la dif entre el numero mas chico y el mas grande
dispersion :: Number -> Number -> Number -> Number
dispersion medida1 medida2 medida3 = maxDe3 medida1 medida2 medida3 - minDe3 medida1 medida2 medida3

--b
diasParejos :: Number -> Number -> Number -> Bool
diasParejos medida1 medida2 medida3 = dispersion medida1 medida2 medida3 < 30

diasLocos :: Number -> Number -> Number -> Bool
diasLocos medida1 medida2 medida3 = dispersion medida1 medida2 medida3 > 100

diasNormales :: Number -> Number -> Number -> Bool
diasNormales medida1 medida2 medida3 = not (diasLocos medida1 medida2 medida3) && not (diasParejos medida1 medida2 medida3)

--punto 11 | significa si cumple esa condicion entonces...
--a
pesoPino :: Number -> Number
pesoPino altura   
    | altura <= 300 = altura *3
    | altura > 300 = (altura - 300) * 2 + 900  --tmb podria ser 300*3 + (altura-300)*2

--b
esPesoUtil :: Number -> Bool
esPesoUtil peso = peso >= 400 && peso <= 1000

--c
sirvePino :: Number -> Bool
sirvePino altura = esPesoUtil (pesoPino altura)

--otra forma de hacer la funcion mas funcional (siempre preferimos esta forma)
sirvePinoV2 :: Number -> Bool
sirvePinoV2 altura= esPesoUtil . pesoPino $ altura  --tmb se podria escribir (esPesoUtil . pesoPino) altura

--punto 12 
funcionAuxiliar :: Number -> Number -> Bool
funcionAuxiliar numero1 numero2 
    | numero1 ==0 = True
    | numero1 < 0 = False
    | numero1 > 0 = funcionAuxiliar (numero1 - (2 * numero2 + 1)) (numero2 + 1)

esCuadradoPerfecto :: Number -> Bool 
esCuadradoPerfecto numero = funcionAuxiliar numero 0 





--Funcional 2: Aplicación Parcial y Composición
--Aplicación parcial
--1 
siguiente :: Number -> Number
siguiente = (+1)     -- no se le pone siguiente numero = numero +1 mas 1 ya es funcion 

--2
mitad :: Number -> Number
mitad = (/2)

--3
inversa :: Number -> Number
inversa = (1/)

--4
triple :: Number -> Number
triple = (*3)

--5
esNumeroPositivo :: Number -> Bool
esNumeroPositivo = (>0)

--Composición

--6 Aplicacion parcial + composicion: devuelve ture si el segundo es multiplo del primero
esMultiploDeV2 :: Number -> Number -> Bool
esMultiploDeV2 numero = (== 0) . (`mod` numero) 

--7 parcial + composicion devuelve true si el año es bisiesto
-- esBisiesto numero = esMultiploDe 400 numero || esMultiploDe 4 numero && not (esMultiploDe 100 numero)
-- no tengo idea de como hacerlo de forma parcial y con composicion

--8

--9

--10