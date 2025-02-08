module FizzBuzz where

-- recibe entero y devuelve String
fizzBuzz :: Int -> String
fizzBuzz numero
    | numero `mod` 15 == 0 = "FizzBuzz!"  -- Si es divisible por 15
    | numero `mod` 3  == 0 = "Fizz!"     -- Si es divisible por 3
    | numero `mod` 5  == 0 = "Buzz!"     -- Si es divisible por 5
    | otherwise       = numeroPalabras numero  -- Si no convierte en palabras

-- convierte entre 1 y 19 en palabras
lessThan20 :: Int -> String           
lessThan20 n
        | n > 0 && n < 20 =  -- si está entre 1 y 19
        let respuestas = words ("one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen")
        in respuestas !! (n - 1)  -- palabra correspondiente a n

-- convierte mayores de 19
tens :: Int -> String
tens n 
    | n > 1 && n <= 9 =  -- si entre 20 y 90
        respuestas !! (n - 2)  -- Devuelve palabra
        where
            respuestas = words "twenty thirty forty fifty sixty seventy eighty ninety"

-- convierte a palabras si no es modulo
numeroPalabras :: Int -> String
numeroPalabras n 
    | n > 0 && n < 20 = lessThan20 n  -- Si esta entre 1 y 19 llama lessThan20
    | n `mod` 10 == 0 && n < 100  = tens (n `div` 10)  -- Si es múltiplo de 10 y menor que 100, llama tens
    | n < 100 = tens (n `div` 10) ++ " " ++ lessThan20 (n `mod` 10)  -- Si es menor que 100
    | n == 100 = "one hundred!"  -- Si es 100
    | otherwise = show n  -- Si es mayor que 100

-- main
main :: IO ()
main = mapM_ (putStrLn . fizzBuzz) [1..27]
