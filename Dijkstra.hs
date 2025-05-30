import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

type Nodo = String
type Peso = Int
type Grafo = Map.Map Nodo [(Nodo, Peso)]
type Distancias = Map.Map Nodo Int
type Caminos = Map.Map Nodo Nodo
type Visitados = Set.Set Nodo

-- Algoritmo de Dijkstra
dijkstra :: Grafo -> Nodo -> (Distancias, Caminos)
dijkstra grafo inicio = dijkstraAux grafo (Map.singleton inicio 0) Set.empty Map.empty

dijkstraAux :: Grafo -> Distancias -> Visitados -> Caminos -> (Distancias, Caminos)
dijkstraAux grafo dist visitados caminos
  | Map.null noVisitados = (dist, caminos)
  | otherwise = dijkstraAux grafo dist' visitados' caminos'
  where
    noVisitados = Map.withoutKeys dist visitados
    actual = fst $ minimumBy (comparing snd) (Map.toList noVisitados)
    vecinos = fromMaybe [] (Map.lookup actual grafo)
    (dist', caminos') = foldl (actualizar actual) (dist, caminos) vecinos
    visitados' = Set.insert actual visitados

actualizar :: Nodo -> (Distancias, Caminos) -> (Nodo, Peso) -> (Distancias, Caminos)
actualizar actual (dist, caminos) (vecino, peso) =
  let nuevaDist = Map.findWithDefault maxBound actual dist + peso
      distVecino = Map.findWithDefault maxBound vecino dist
  in if nuevaDist < distVecino
       then (Map.insert vecino nuevaDist dist, Map.insert vecino actual caminos)
       else (dist, caminos)

-- Ejemplo de grafo
grafoEjemplo :: Grafo
grafoEjemplo = Map.fromList
  [ ("A", [("B", 4), ("C", 2)])
  , ("B", [("C", 1), ("D", 5)])
  , ("C", [("D", 8), ("E", 10)])
  , ("D", [("E", 2), ("Z", 6)])
  , ("E", [("Z", 3)])
  , ("Z", [])
  ]

-- Mostrar resultados
main :: IO ()
main = do
  let inicio = "A"
  let (distancias, caminos) = dijkstra grafoEjemplo inicio
  putStrLn $ "Distancias desde el nodo " ++ inicio ++ ":"
  mapM_ (\(n, d) -> putStrLn (n ++ ": " ++ show d)) (Map.toList distancias)