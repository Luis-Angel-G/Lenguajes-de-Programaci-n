import qualified Data.Map as Map
import Data.Char (isAlpha, toLower)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

-- Función principal
main :: IO ()
main = do
  putStrLn "Ingresa un texto:"
  texto <- getContents
  let palabrasLimpias = limpiar texto
      totalPalabras = length palabrasLimpias
      frecuencias = contarFrecuencias palabrasLimpias
      top5 = take 5 (ordenarFrecuencias frecuencias)

  putStrLn $ "Total de palabras: " ++ show totalPalabras
  putStrLn "Top 5 palabras más comunes:"
  mapM_ (\(w, c) -> putStrLn (w ++ ": " ++ show c)) top5

-- Limpia el texto: lo convierte a minúsculas, elimina signos y separa palabras
limpiar :: String -> [String]
limpiar = words . map (\c -> if isAlpha c || c == ' ' then toLower c else ' ')

-- Cuenta las veces que aparece cada palabra
contarFrecuencias :: [String] -> Map.Map String Int
contarFrecuencias = foldr (\palabra acc -> Map.insertWith (+) palabra 1 acc) Map.empty

-- Ordena el mapa de frecuencias de mayor a menor
ordenarFrecuencias :: Map.Map String Int -> [(String, Int)]
ordenarFrecuencias = sortOn (Down . snd) . Map.toList