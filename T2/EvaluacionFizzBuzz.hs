module FizzBuzz where

-- Función principal
fizzBuzz :: Int -> String
fizzBuzz n
    | esPrimo n = "FizzBuzz!"
    | otherwise = numeroPalabras n

-- verificar si es primo
esPrimo :: Int -> Bool
esPrimo n
    | n < 2     = False
    | n == 2    = True
    | even n    = False
    | otherwise = null [ x | x <- [3,5..(floor(sqrt (fromIntegral n)))], n `mod` x == 0 ]

-- Conversion a palabras
numeroPalabras :: Int -> String
numeroPalabras 0 = "cero"
numeroPalabras 1000000 = "un millón"
numeroPalabras n
    | n < 20    = unidades !! (n - 1)
    | n < 100   = if n `mod` 10 == 0 then decenas !! (n `div` 10 - 2)
                  else decenas !! (n `div` 10 - 2) ++ " y " ++ unidades !! (n `mod` 10 - 1)
    | n < 1000  = if n `mod` 100 == 0 then centenas !! (n `div` 100 - 1)
                  else centenas !! (n `div` 100 - 1) ++ " " ++ numeroPalabras (n `mod` 100)
    | n < 10000 = if n `mod` 1000 == 0 then numeroPalabras (n `div` 1000) ++ " mil"
                  else numeroPalabras (n `div` 1000) ++ " mil " ++ numeroPalabras (n `mod` 1000)
    | n < 1000000 = if n `mod` 100000 == 0 then numeroPalabras (n `div` 100000) ++ " cien mil"
                    else numeroPalabras (n `div` 1000) ++ " mil " ++ numeroPalabras (n `mod` 1000)
    | n < 100000000 = if n `mod` 1000000 == 0 then numeroPalabras (n `div` 1000000) ++ " millones"
                      else numeroPalabras (n `div` 1000000) ++ " millones " ++ numeroPalabras (n `mod` 1000000)
    | otherwise = "No adecuado"
  where
    unidades = ["uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve",
                "diez", "once", "doce", "trece", "catorce", "quince", "dieciséis", "diecisiete", "dieciocho", "diecinueve"]

    decenas  = ["veinte", "treinta", "cuarenta", "cincuenta", "sesenta", "setenta", "ochenta", "noventa"]

    centenas = ["cien", "doscientos", "trescientos", "cuatrocientos", "quinientos", "seiscientos", "setecientos", "ochocientos", "novecientos"]

main :: IO ()
main = do
    print $ fizzBuzz 7451232
    print $ fizzBuzz 53
