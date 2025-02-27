module Ejercicios where
import Data.Char (toLower)

--Suma de elementos
sumar :: [Int] -> Int
sumar = sum

--Factorial
factorial :: Int -> Int
factorial 0 = 1
factorial n = n*factorial (n-1)

--Números pares
numerosPares :: Int -> [Int]
numerosPares n = [x | x <- [0..n], even x]

--Longitud de una cadena
longitudCadena :: String -> Int
longitudCadena = length

--Reverso de una lista
reverso :: [a] -> [a]
reverso = reverse

--Duplicar elementos
duplicar :: [Int] -> [Int]
duplicar = map (*2)

--Filtrar pares
pares :: [Int] -> [Int]
pares = filter even

--Fibonacci [el indice comienza en 0]
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

--Divisores
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

--Palíndromo
palindromo :: String -> Bool
palindromo s = let s' = (map toLower s) in s' == reverse s'

--Palíndromo funciona con espacios
palindromoEspacios :: String -> Bool
palindromoEspacios s = let s' = filter (/= ' ') (map toLower s) in s' == reverse s'
