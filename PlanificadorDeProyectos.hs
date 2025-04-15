-- Importamos el módulo Data.Map con un alias para trabajar con mapas (estructuras clave-valor).
import qualified Data.Map as Map
import Data.Map (Map)

-- Importamos la función replicateM del módulo Control.Monad para realizar acciones repetitivas en el contexto de IO.
import Control.Monad (replicateM)

-- Definimos un alias para representar el nombre de una tarea como un String.
type Tarea = String

-- Definimos un alias para representar la duración de una tarea como un Int.
type Duracion = Int

-- Definimos un alias para representar un proyecto como un mapa donde:
-- - La clave es el nombre de la tarea (Tarea).
-- - El valor es un par que contiene la duración de la tarea (Duracion) y una lista de dependencias (otras tareas de las que depende).
type Proyecto = Map Tarea (Duracion, [Tarea])

-- Función que calcula el tiempo total necesario para completar un proyecto.
-- Toma como entrada un proyecto (de tipo Proyecto) y devuelve un entero (el tiempo total en días).
tiempoTotal :: Proyecto -> Int
tiempoTotal proyecto = maximum (map (tiempoTarea []) (Map.keys proyecto))
  where
    -- Función auxiliar que calcula el tiempo necesario para completar una tarea específica.
    -- Lleva un registro de las tareas ya visitadas para evitar ciclos.
    tiempoTarea visitados tarea
      -- Si la tarea ya fue visitada, se retorna 0 para evitar ciclos infinitos.
      | tarea `elem` visitados = 0
      | otherwise =
          case Map.lookup tarea proyecto of
            -- Si la tarea no está en el proyecto, se retorna 0.
            Nothing -> 0
            -- Si la tarea está en el proyecto, se suma su duración y el tiempo de sus dependencias.
            Just (duracion, dependencias) ->
              duracion + sum (map (tiempoTarea (tarea : visitados)) dependencias)

-- Función que permite al usuario ingresar los datos de una tarea desde la consola.
-- Devuelve un par donde:
-- - La clave es el nombre de la tarea.
-- - El valor es un par con la duración de la tarea y una lista de dependencias.
leerTarea :: IO (Tarea, (Duracion, [Tarea]))
leerTarea = do
  -- Pedimos al usuario el nombre de la tarea.
  putStrLn "\nNombre de la tarea:"
  nombre <- getLine

  -- Pedimos al usuario la duración de la tarea en días.
  putStrLn "Duración de la tarea (en días):"
  duracionStr <- getLine
  let duracion = read duracionStr :: Int

  -- Pedimos al usuario las dependencias de la tarea, separadas por comas.
  putStrLn "Dependencias (separadas por coma, o deja vacío si no hay):"
  dependenciasStr <- getLine

  -- Procesamos las dependencias ingresadas por el usuario.
  let dependencias = if null dependenciasStr then [] else map trim (splitComas dependenciasStr)

  -- Retornamos la tarea con su duración y dependencias.
  return (nombre, (duracion, dependencias))

-- Función auxiliar que elimina los espacios en blanco al inicio y al final de una cadena.
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile (== ' ')

-- Función auxiliar que divide una cadena en una lista de cadenas, usando la coma como separador.
splitComas :: String -> [String]
splitComas s = case break (== ',') s of
  -- Si no hay más comas, retornamos la cadena restante como un único elemento.
  (x, []) -> [x]
  -- Si hay una coma, dividimos la cadena en dos partes y procesamos recursivamente la parte restante.
  (x, _:rest) -> x : splitComas rest

-- Función principal del programa.
main :: IO ()
main = do
  -- Pedimos al usuario cuántas tareas desea ingresar.
  putStrLn "¿Cuántas tareas quieres ingresar?"
  numeroStr <- getLine
  let numero = read numeroStr :: Int

  -- Usamos replicateM para leer las tareas repetidamente según el número ingresado.
  tareas <- replicateM numero leerTarea

  -- Creamos un proyecto a partir de las tareas ingresadas, usando Map.fromList.
  let proyecto = Map.fromList tareas

  -- Calculamos el tiempo total del proyecto.
  putStrLn "\nCalculando tiempo total del proyecto..."
  let total = tiempoTotal proyecto

  -- Mostramos el tiempo total estimado al usuario.
  putStrLn $ "Tiempo total estimado: " ++ show total ++ " días"