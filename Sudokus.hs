{- El programa recibe un sudoku a resolver en un fichero donde cada fila es una línea 
y no hay espacios entre las casillas. Cada espacio en blanco se representa como un 0, 
en caso de querer generar un sudoku (ya resuelto) se le debe dar un sudoku vacío (todo 0).
El programa generará un fichero de salida con la/s resolución/es del sudoku proporcionado.
La práctica está comentada explicando qué hace cada función empleada.-}

-- Definición de tipos, al final no hay data
-- La idea era implementar una data que permitiese pintar el sudoku por pantalla, pero se nos atragantó.
import Data.List
type Sudoku a = [Fila a] 
type Cuadricula = Sudoku Valor

type Valor = Char
type Posibilidades  = [Valor]

type Fila a = [a]
type Columna a = [a]
type Caja a = [a]

-- Funciones para, dado un sudoku, sacar filas, columnas y cajas

-- Esta función devuelve el sudoku por filas

filas :: Sudoku a -> [Fila a]
filas = id

-- Esta función devuelve el sudoku por columnas

columnas :: Sudoku a -> [Columna a]
columnas = transpose

-- Esta función devuelve el sudoku por cajas (por filas)

-- *función auxiliar* Esta función recibe un natural y una lista y divide la lista en listas de longitud el natural
agrupar_n :: Int -> [a] -> [[a]]
agrupar_n n [] = []
agrupar_n n xs = take n xs : agrupar_n n (drop n xs)

-- *función principal*
cajas :: Sudoku a -> [Caja a]
cajas = dividir . map columnas . agrupar
	where 
		agrupar = (agrupar_n 3). map (agrupar_n 3 )
		{- divido las filas en elementos de 3 en 3 y luego tomo esos elementos también de 3 en 3.
		esto me da una lista de listas de 3 listas de 3 elementos -}
		dividir = map concat. concat 
		{- esta función está para quitar las listas sobrantes por la anterior -}



-- Funciones empleadas para la resolución del problema

-- Devuelve una lista con todos los posibles valores que puede haber en una casilla
valores :: [Valor]
valores = ['1'..'9']

-- Devuelve True si la casilla está vacía (en nuestra implementación aparece como un 0)
vacio :: Valor -> Bool
vacio = (== '0')

-- Devuelve True si la lista contiene un solo elemento. Se puede aplicar a cadenas de caracteres (útil en la implementación)
unico :: [a] -> Bool
unico [_] = True
unico _ = False

-- Comprueba que no se repiten elementos en una lista, en este caso devuelve True
sinRepes :: Eq a => [a] -> Bool
sinRepes [] = True
sinRepes (x:xs) = not(elem x xs) && sinRepes xs

-- Mete en cada casilla vacia la lista entera de valores
rellenar :: Cuadricula -> Sudoku Posibilidades
rellenar =  map (map relleno)
	where
		relleno casilla = if vacio casilla then valores else [casilla] 
		{- esta función está para adaptar el tipo en caso de haber un valor 
		o para rellenar con todos los valores en caso contrario -}

-- Saca en una lista todas las posibles combinaciones tomando 1 elemento de cada lista de las dadas
combinaciones :: [[a]] -> [[a]]
combinaciones [] = [[]]
combinaciones (xs:xss) = [y:ys | y <- xs, ys <- combinaciones xss] 

-- Dada una matriz de casillas de tipo lista, devuelve una lista con todas las posibles matrices tienendo en cada casilla un solo elemento
colapsar :: Sudoku [a] -> [Sudoku a]
colapsar = combinaciones . map combinaciones 

-- *función auxiliar* Devuelve todos los elementos de la primera lista que no estén en la segunda a menos que la primera solo contenga un elemento
salvo :: Posibilidades -> Posibilidades -> Posibilidades
salvo [] ys = []
salvo (x:[]) ys = x:[]
salvo (x:xs) ys
	| elem x ys = salvo xs ys
	|otherwise = x:salvo xs ys

-- *función auxiliar* Dada una fila, columna, caja, quita todos los elementos ya determinados para reducir las opciones de colapso en las casillas
reducir :: Fila Posibilidades -> Fila Posibilidades
reducir xss = [xs `salvo` determinados | xs <- xss]
	where 
		determinados = concat (filter unico xss)

-- *funcion principal* Aplica la función reducir a cada una de las casillas teniendo en cuenta que cada casilla esta en una fila, en una columna y en una caja
reducPos :: Sudoku Posibilidades -> Sudoku Posibilidades
reducPos =  reducSobre cajas . reducSobre columnas . reducSobre filas
	where 
		reducSobre f = f . map reducir . f

-- Devuelve True si todos los valores están determinados (una única opción)
sudokuCompleto :: Sudoku Posibilidades -> Bool
sudokuCompleto = all (all unico)

-- Devuelve True si alguna casilla está vacía (no hay ninguna posibilidad de rellenar alguna casilla)
sudokuIrrellenable :: Sudoku Posibilidades -> Bool
sudokuIrrellenable = any (any null)

-- *función auxiliar* Aplica la función consistente a las filas, columnas y cajas para comprobar que no se repiten elementos
sinDuplicados :: Sudoku Posibilidades -> Bool
sinDuplicados sudoku =  all sinDuplicadosAux (filas sudoku) && all sinDuplicadosAux (columnas sudoku) && all sinDuplicadosAux (cajas sudoku)
	where
		sinDuplicadosAux = sinRepes . concat . filter unico

-- Un sudoku es irresoluble si alguna casilla no se puede rellenar o si se repite algún elemento en una fila, columna o caja
irresoluble :: Sudoku Posibilidades -> Bool
irresoluble sudoku = sudokuIrrellenable sudoku || not (sinDuplicados sudoku)



-- Va rellenando el sudoku paso por paso siempre que sea posible
completar :: Sudoku Posibilidades -> [Cuadricula]
completar sudoku
	| irresoluble sudoku = []
	| sudokuCompleto sudoku = colapsar sudoku
	| otherwise = [g | sudoku' <- colapsoLocal sudoku , g <- completar (reducPos sudoku')]

{- Funciona igual que colapsar pero colapsando un solo elemento para poder ir comprobando en cada momento 
 si sigue siedo resoluble el sudoku (mejora eficiencia al no tener que sacarlos todos y comprobar luego)-}
colapsoLocal :: Sudoku Posibilidades -> [Sudoku Posibilidades]
colapsoLocal sudoku = [filas1 ++ [fila1 ++ [determinado] : fila2] ++ filas2 | determinado <- determinados]
	where
		(filas1,fila:filas2) = break(any(not . unico)) sudoku
		{- break devuelve un par con 2 listas. Aplica la función que recibe como parámetro
		a cada elemento de la lista proporcionada y toma como punto de corte el primer true que devuelve-}
		(fila1,determinados:fila2)= break(not . unico) fila

-- Resuelve el sudoku proporcionado si es posible, en caso contrario devuelve una lista vacia
solucion :: Cuadricula -> [Cuadricula]
solucion = completar . reducPos . rellenar

main :: IO ()
main = do putStr "Dame el nombre del fichero de entrada \n"
          inNombre <- getLine
          contenido <- readFile inNombre
          putStr "Dame el nombre del fichero de salida \n"
          outNombre <- getLine
	  putStr "Si quieres generar sudokus, escribe cuántos, en caso contrario, escibe 1 \n"
          n <- getLine
          let salida = concat (take (read n::Int) (solucion (lines contenido))) 
          writeFile outNombre (unlines salida)
