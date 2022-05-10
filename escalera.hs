-- Tienes que subir N peldaños de una escalera. Y lo puedes hacer en pasos de 1
-- en 1 o de 2 en 2. ¿De cuántas formas puedes subir una escalera si combinas
-- dar un paso o dos pasos?

import qualified Data.Set as Set

-- formas 5 [1, 2] => si puedo subir los escalones de 1 en 1 o de 2 en 2, de
-- cuantas formas puedo subir 5 escalones.
-- formas 10 [1, 3, 5] => si puedo subir los escalones de 1 en 1, de 3 en 3
-- o de 5 en 5, de cuantas formas puedo subir 10 escalones
formas :: Int -> [Int] -> Int
formas n x =
  let formas_totales = Set.fromList (hformas n x [])
      formas_validas = Set.filter (\f -> (sum f) == n) formas_totales
  in  Set.size formas_validas

hformas :: Int -> [Int] -> [Int] -> [[Int]]
hformas n x camino = (concat todos)
  where todos = (map (\p -> if n >= p then (hformas (n - p) x (p : camino)) else [camino]) x)
        

