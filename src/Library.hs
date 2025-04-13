module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

saludo :: String -> String
saludo nombre = "Hola, " ++ nombre ++ "! Bienvenida a Haskell ✨"

miFuncion :: Number -> Bool
miFuncion 1 = True