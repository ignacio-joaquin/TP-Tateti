import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)
import System.Exit

-- Tipos de datos
type Fila = [Char]
type Tablero = [Fila]
type Posicion = (Int, Int)

-- Parsers para entrada del usuario
parserColumna :: Parser Int
parserColumna = do
    n <- natural
    if n >= 1 then return n else failure

parserTablero :: Parser (Int, Int)
parserTablero = do
    symbol "Filas:"
    filas <- natural
    symbol "Columnas:"
    columnas <- natural
    if filas >= 3 && columnas >= 3 
        then return (filas, columnas)
        else failure

-- Función para parsear entrada
parseInput :: Parser a -> String -> Maybe a
parseInput p input = 
    case parse (p <* eof) input of
        [(result, _)] -> Just result
        _ -> Nothing
  where
    eof = P (\inp -> if null inp then [((), "")] else [])

-- Impresión del tablero
printearBordeVertical :: Int -> IO()
printearBordeVertical 1 = putStr "-----\n"
printearBordeVertical n = do
    putStr "-----" 
    printearBordeVertical (n-1)

printearTablero :: Int -> Tablero -> IO()
printearTablero _ [] = putStrLn ""
printearTablero n (x:xs) = do 
    printearBordeVertical n
    printearFila n x
    printearTablero n xs 

printearFila :: Int -> Fila -> IO()
printearFila 0 _ = putStrLn ""
printearFila n (x:xs) = do 
    putStr "| "
    putChar x
    putStr " |"
    printearFila (n-1) xs

-- Construcción del tablero
construirTablero :: IO Tablero
construirTablero = do
    putStrLn "=== 3 EN LÍNEA ==="
    putStrLn "Ingresa las medidas del tablero (ej: 'Filas: 3 Columnas: 4'):"
    input <- getLine
    let resultado = parseInput parserTablero input
    if isJust resultado
        then do
            let Just (n, m) = resultado
            putStrLn $ "Creando tablero " ++ show n ++ "x" ++ show m
            return (construirTableroVacio n m)
        else do
            putStrLn "Formato inválido o dimensiones menores a 3. Usa: 'Filas: N Columnas: M' con N,M ≥ 3"
            construirTablero
  where
    isJust (Just _) = True
    isJust Nothing = False

construirTableroVacio :: Int -> Int -> Tablero
construirTableroVacio filas columnas = replicate filas (replicate columnas ' ')

-- Lógica del juego
juegoLoop :: Tablero -> Char -> IO ()
juegoLoop tablero jugador = do
    let columnas = length (head tablero)
    putStrLn $ "\nTurno del jugador: " ++ [jugador]
    
    nuevoTablero <- jugarTurno tablero jugador
    
    -- Verificar si hay ganador
    let ganador = verificarVictoria nuevoTablero 'X' <|> verificarVictoria nuevoTablero 'O'
    if isJust ganador
        then do
            putStrLn "\n=== TABLERO FINAL ==="
            printearTablero columnas nuevoTablero
            let Just g = ganador
            putStrLn $ "¡Jugador " ++ [g] ++ " gana!"
            reiniciarJuego
        else do
            let siguienteJugador = if jugador == 'X' then 'O' else 'X'
            juegoLoop nuevoTablero siguienteJugador
  where
    isJust (Just _) = True
    isJust Nothing = False

jugarTurno :: Tablero -> Char -> IO Tablero
jugarTurno tablero jugador = do
    let columnas = length (head tablero)
    putStrLn "\n=== TABLERO ACTUAL ==="
    printearTablero columnas tablero
    putStr $ "Jugador " ++ [jugador] ++ ", ingresa el número de columna (1-" ++ show columnas ++ "): \n"
    colStr <- getLine
    
    let colParseada = parseInput parserColumna colStr
    if isJust colParseada
        then do
            let Just col = colParseada
            let nuevoTablero = dropFicha tablero col jugador
            if isJust nuevoTablero
                then do
                    let Just tab = nuevoTablero
                    return tab
                else do
                    putStrLn "Columna inválida o llena! Intenta otra."
                    jugarTurno tablero jugador
        else do
            putStrLn "Número inválido! Ingresa un número válido."
            jugarTurno tablero jugador
  where
    isJust (Just _) = True
    isJust Nothing = False

dropFicha :: Tablero -> Int -> Char -> Maybe Tablero
dropFicha tablero col ficha
    | col < 1 || col > length (head tablero) = Nothing
    | otherwise = buscarFila (length tablero - 1) tablero
    where
        colIndex = col - 1
        
        buscarFila (-1) _ = Nothing
        buscarFila filaIdx tab
            | (tab !! filaIdx) !! colIndex == ' ' = 
                Just (actualizarTablero tab filaIdx colIndex ficha)
            | otherwise = buscarFila (filaIdx - 1) tab

actualizarTablero :: Tablero -> Int -> Int -> Char -> Tablero
actualizarTablero [] _ _ _ = []
actualizarTablero (fila:filas) 0 col ficha = 
    actualizarFila fila col ficha : filas
actualizarTablero (fila:filas) n col ficha = 
    fila : actualizarTablero filas (n-1) col ficha

actualizarFila :: Fila -> Int -> Char -> Fila
actualizarFila [] _ _ = []
actualizarFila (c:cs) 0 ficha = ficha : cs
actualizarFila (c:cs) n ficha = c : actualizarFila cs (n-1) ficha

-- Verificación de victoria usando <|>
verificarVictoria :: Tablero -> Char -> Maybe Char
verificarVictoria tablero jugador
    | verificarHorizontal tablero jugador = Just jugador
    | verificarVertical tablero jugador = Just jugador
    | verificarDiagonales tablero jugador = Just jugador
    | otherwise = Nothing

-- Verificar 3 en línea horizontal
verificarHorizontal :: Tablero -> Char -> Bool
verificarHorizontal tablero jugador = any (verificarFila jugador) tablero
  where
    verificarFila jugador fila = 
        any (\i -> all (\j -> fila !! (i+j) == jugador) [0..2]) [0..length fila - 3]

-- Verificar 3 en línea vertical
verificarVertical :: Tablero -> Char -> Bool
verificarVertical tablero jugador = 
    any verificarColumna [0..length (head tablero) - 1]
  where
    verificarColumna col =
        any (\fila -> all (\i -> tablero !! (fila+i) !! col == jugador) [0..2]) [0..length tablero - 3]

-- Verificar 3 en línea diagonales
verificarDiagonales :: Tablero -> Char -> Bool
verificarDiagonales tablero jugador = 
    verificarDiagonalesDesc tablero jugador || verificarDiagonalesAsc tablero jugador

-- Diagonales descendentes (\)
verificarDiagonalesDesc :: Tablero -> Char -> Bool
verificarDiagonalesDesc tablero jugador =
    any (\(fila, col) -> 
        all (\i -> tablero !! (fila+i) !! (col+i) == jugador) [0..2])
    [(f, c) | f <- [0..length tablero - 3], c <- [0..length (head tablero) - 3]]

-- Diagonales ascendentes (/)
verificarDiagonalesAsc :: Tablero -> Char -> Bool
verificarDiagonalesAsc tablero jugador =
    any (\(fila, col) -> 
        all (\i -> tablero !! (fila+i) !! (col+2-i) == jugador) [0..2])
    [(f, c) | f <- [0..length tablero - 3], c <- [0..length (head tablero) - 3]]

-- Reiniciar el juego usando pattern matching simple
reiniciarJuego :: IO ()
reiniciarJuego = do
    putStrLn "\n¿Quieres jugar otra vez? (s/n): "
    respuesta <- getLine
    if respuesta == "s" 
        then do
            putStrLn "\n" 
            iniciarJuego
        else if respuesta == "n"
            then putStrLn "¡Gracias por jugar!"
            else do
                putStrLn "Respuesta inválida. Ingresa 's' o 'n'."
                reiniciarJuego

-- Iniciar juego completo
iniciarJuego :: IO ()
iniciarJuego = do
    tablero <- construirTablero
    juegoLoop tablero 'X'

-- Función principal
main :: IO ()
main = iniciarJuego

-- Función auxiliar para Maybe con <|>
orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing y = y
