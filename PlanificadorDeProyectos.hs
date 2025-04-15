import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad (replicateM)

type Tarea = String
type Duracion = Int
type Proyecto = Map Tarea (Duracion, [Tarea])

tiempoTotal :: Proyecto -> Int
tiempoTotal proyecto = maximum (map (tiempoTarea []) (Map.keys proyecto))
  where
    tiempoTarea visitados tarea
      | tarea `elem` visitados = 0
      | otherwise =
          case Map.lookup tarea proyecto of
            Nothing -> 0
            Just (duracion, dependencias) ->
              duracion + sum (map (tiempoTarea (tarea : visitados)) dependencias)

leerTarea :: IO (Tarea, (Duracion, [Tarea]))
leerTarea = do
  putStrLn "\nNombre de la tarea:"
  nombre <- getLine
  putStrLn "Duración de la tarea (en días):"
  duracionStr <- getLine
  let duracion = read duracionStr :: Int
  putStrLn "Dependencias (separadas por coma, o deja vacío si no hay):"
  dependenciasStr <- getLine
  let dependencias = if null dependenciasStr then [] else map trim (splitComas dependenciasStr)
  return (nombre, (duracion, dependencias))

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile (== ' ')

splitComas :: String -> [String]
splitComas s = case break (== ',') s of
  (x, []) -> [x]
  (x, _:rest) -> x : splitComas rest

main :: IO ()
main = do
  putStrLn "¿Cuántas tareas quieres ingresar?"
  numeroStr <- getLine
  let numero = read numeroStr :: Int
  tareas <- replicateM numero leerTarea
  let proyecto = Map.fromList tareas
  putStrLn "\nCalculando tiempo total del proyecto..."
  let total = tiempoTotal proyecto
  putStrLn $ "Tiempo total estimado: " ++ show total ++ " días"