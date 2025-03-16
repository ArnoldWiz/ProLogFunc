import Data.List
import Data.Char
import qualified Data.Map as Map
import Data.Maybe

--descuento,iva
descuento :: Double -> Double -> Double
descuento precio d = precio * (1 - d / 100)

iva :: Double -> Double -> Double
iva precio p = precio * ( 1 + p / 100)

cesta :: Map.Map String (Double, Double) -> (Double -> Double -> Double) -> Double
cesta c funcion = sum [funcion precio porcentaje | (precio, porcentaje) <- Map.elems c]

--función a cada elemento de una lista
lista :: (a -> b) -> [a] -> [b]
lista f l = map f l

--diccionario con palabras y su longitud
longitud :: String -> Map.Map String Int
longitud f = Map.fromList [(palabra, length palabra) | palabra <- words f]

--notas a calificaciones
calificacion :: Double -> String
calificacion n
    | n >= 95 = "Excelente"
    | n >= 85 = "Notable"
    | n >= 75 = "Bueno"
    | n >= 70 = "Suficiente"
    | otherwise  = "Desempeño insuficiente"

asignaturas :: Map.Map String Double -> Map.Map String String
asignaturas n = Map.mapKeys (map toUpper) (Map.map calificacion n)

--módulo vector
modulo :: [Double] -> Double
modulo v = sqrt (sum (map (^2) v))


--atipico
media :: [Double] -> Double
media lista = sum lista / fromIntegral (length lista)

desvEstandar :: [Double] -> Double
desvEstandar lista = sqrt (sum [(x - m)^2 | x <- lista] / fromIntegral (length lista))
  where m = media lista

valoresAtipicos :: [Double] -> [Double]
valoresAtipicos lista = [x | x <- lista, abs ((x - m) / dt) > 2]  
  where
    m = media lista
    dt = desvEstandar lista


-- Pruebas
main :: IO ()
main = do
    let productos = Map.fromList [("producto1", (100, 10))]

    putStrLn "Total de la cesta:"
    print (cesta productos iva)

    putStrLn "Lista funcion: "
    print (lista (*2) [1,2,3,4])

    putStrLn "diccionario: "
    print (longitud "Hola como estan?")

    let n = Map.fromList [("Matematicas", 90), ("Historia", 75)]
    putStrLn "calificaciones: "
    print (asignaturas n)

    putStrLn "vector: "
    print (modulo [3, 4])

    putStrLn "Ejemplos de valores atípicos:"
    print $ valoresAtipicos [1, 2, 3, 4, 5, 100]