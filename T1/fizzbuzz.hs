import Data.List (intercalate)

-- Función principal que recibe un número y devuelve la cadena correspondiente
fizzBuzz :: Int -> String
fizzBuzz numero
    | numero `mod` 15 == 0 = "FizzBuzz!"
    | numero `mod` 3  == 0 = "Fizz!"
    | numero `mod` 5  == 0 = "Buzz!"
    | otherwise       = numeroEnPalabras numero

-- Arreglos para la conversión de números a palabras
unidades :: [String]
unidades = ["zero!", "one!", "two!", "three!", "four!", "five!", "six!", "seven!", "eight!", "nine!", "ten!", 
            "eleven!", "twelve!", "thirteen!", "fourteen!", "fifteen!", "sixteen!", "seventeen!", "eighteen!", "nineteen!"]

decenas :: [String]
decenas = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

-- Función para convertir números a palabras en inglés usando arreglos
numeroEnPalabras :: Int -> String
numeroEnPalabras numero
    | numero < 20 = unidades !! numero
    | numero < 100 = decenas !! (numero `div` 10) ++ if numero `mod` 10 /= 0 then "-" ++ unidades !! (numero `mod` 10) else ""
    | numero == 100 = "one hundred!"
    | otherwise = show numero

-- Función principal para pruebas
main :: IO ()
main = mapM_ (putStrLn . fizzBuzz) [1..100]