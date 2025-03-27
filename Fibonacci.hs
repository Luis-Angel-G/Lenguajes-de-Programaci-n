-- Definimos una función recursiva para calcular el factorial de un número
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Definimos una función recursiva para calcular la serie de Fibonacci
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Generamos la lista de los primeros n números de Fibonacci
fibonacciSeries :: Integer -> [Integer]
fibonacciSeries n = map fibonacci [0..n-1]

-- Función principal que interactúa con el usuario
main :: IO ()
main = do
  putStrLn "Ingresa un número para calcular su factorial:"
  input1 <- getLine
  let num1 = read input1 :: Integer
  putStrLn ("El factorial de " ++ show num1 ++ " es " ++ show (factorial num1))

  putStrLn "Ingresa un número para generar la serie de Fibonacci:"
  input2 <- getLine
  let num2 = read input2 :: Integer
  putStrLn ("La serie de Fibonacci hasta " ++ show num2 ++ " es: " ++ show (fibonacciSeries num2))