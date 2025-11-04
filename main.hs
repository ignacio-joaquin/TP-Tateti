import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)
import System.Exit
import Control.Applicative hiding (many)
import System.Console.ANSI (clearScreen)

-- Tipos de datos
type Fila = [Char]
type Tablero = [Fila]
type Posicion = (Int, Int)
type ResultadoPartida = (Int, Char)  -- (Número de partida, Ganador)
type Estado = [ResultadoPartida]     -- Lista de resultados

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

parseInput :: Parser a -> String -> Maybe a
parseInput p input = 
    case parse parser input of
        [(result, _)] -> Just result
        _ -> Nothing
    where
        parser = do
            result <- p
            eof
            return result
        eof = P (\inp -> if null inp then [((), "")] else [])
-- Impresión del tablero
printearBordeVertical :: Int -> IO()
printearBordeVertical 1 = putStr "-----\n"
printearBordeVertical n = do
    putStr "-----" 
    printearBordeVertical (n-1)

printearTablero :: Int -> Tablero -> IO()
printearTablero n [] = printearBordeVertical n
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

-- Función para verificar si el tablero está lleno
tableroLleno :: Tablero -> Bool
tableroLleno = all (notElem ' ')
-- Explicación:
-- notElem ' ' fila: Verifica que no haya espacios en una fila
-- all: Verifica que todas las filas cumplan esta condición

-- Lógica del juego (ahora recibe y retorna el estado)
juegoLoop :: Estado -> Tablero -> Char -> IO Estado
juegoLoop estado tablero jugador = do
    clearScreen
    let columnas = length (head tablero)
    putStrLn $ "\nTurno del jugador: " ++ [jugador]
    
    nuevoTablero <- jugarTurno estado tablero jugador
    
    -- Verificar si hay ganador
    let ganador = verificarVictoria nuevoTablero 'X' <|> verificarVictoria nuevoTablero 'O'
    if isJust ganador
        then do
            putStrLn "\n=== TABLERO FINAL ==="
            printearTablero columnas nuevoTablero
            let Just g = ganador
            putStrLn $ "¡Jugador " ++ [g] ++ " gana!"
            -- Crear nuevo resultado y retornar estado actualizado
            let numeroPartida = length estado + 1
            let nuevoResultado = (numeroPartida, g)
            let nuevoEstado = nuevoResultado : estado
            reiniciarJuego nuevoEstado
        -- Verificar si hay empate
        else if tableroLleno nuevoTablero
            then do
                putStrLn "\n=== TABLERO FINAL ==="
                printearTablero columnas nuevoTablero
                putStrLn "¡EMPATE! No hay movimientos disponibles."
                -- Crear nuevo resultado de empate y retornar estado actualizado
                let numeroPartida = length estado + 1
                let nuevoResultado = (numeroPartida, 'E')  -- 'E' para empate
                let nuevoEstado = nuevoResultado : estado
                reiniciarJuego nuevoEstado
            else do
                let siguienteJugador = if jugador == 'X' then 'O' else 'X'
                juegoLoop estado nuevoTablero siguienteJugador
  where
    isJust (Just _) = True
    isJust Nothing = False

jugarTurno :: Estado -> Tablero -> Char -> IO Tablero
jugarTurno estado tablero jugador = do
    let columnas = length (head tablero)
    putStrLn "\n=== TABLERO ACTUAL ==="
    printearTablero columnas tablero
    putStr $ "Jugador " ++ [jugador] ++ ", ingresa el número de columna (1-" ++ show columnas ++ ") o ingrese \"menu\" para volver al menu: \n"
    colStr <- getLine
    if colStr == "menu" || colStr == "MENU"
        then do
            putStrLn "Volviendo al menú principal..."
            _ <- menu estado  -- Ignoramos el nuevo estado porque no cambió
            return tablero
        else do
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
                        jugarTurno estado tablero jugador
            else do
                putStrLn "Número inválido! Ingresa un número válido."
                jugarTurno estado tablero jugador
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

-- Reiniciar el juego (ahora recibe y retorna estado)
reiniciarJuego :: Estado -> IO Estado
reiniciarJuego estado = do
    putStrLn "\n¿Quieres volver al menu? (s/n): "
    respuesta <- getLine
    if respuesta == "s" 
        then do
            putStrLn "\n" 
            menu estado
        else if respuesta == "n"
            then do
                putStrLn "¡Gracias por jugar!"
                return estado
            else do
                putStrLn "Respuesta inválida. Ingresa 's' o 'n'."
                reiniciarJuego estado

-- Iniciar juego completo (ahora recibe y retorna estado)
iniciarJuego :: Estado -> IO Estado
iniciarJuego estado = do
    tablero <- construirTablero
    juegoLoop estado tablero 'X'

-- Función para mostrar últimos 5 resultados
mostrarUltimosResultados :: Estado -> IO Estado
mostrarUltimosResultados estado = do
    clearScreen
    putStrLn "=== ÚLTIMOS 5 RESULTADOS ==="
    putStrLn "Partida | Resultado"
    putStrLn "-------------------"
    
    let ultimos5 = take 5 estado
    
    if null ultimos5
        then putStrLn "No hay resultados disponibles."
        else mapM_ imprimirResultado (reverse ultimos5)
    
    putStrLn "\nPresiona Enter para volver al menú..."
    _ <- getLine
    return estado  -- Retornamos el mismo estado (no cambió)
  where
    imprimirResultado (num, resultado) = 
        case resultado of
            'E' -> putStrLn $ "   " ++ show num ++ "    |   EMPATE"
            _   -> putStrLn $ "   " ++ show num ++ "    |   Ganador " ++ [resultado]

-- Menú principal (ahora recibe y retorna estado)
menu :: Estado -> IO Estado
menu estado = do
    clearScreen
    putStrLn "=== 3 EN LÍNEA ==="
    putStrLn "1. Nuevo juego"
    putStrLn "2. Ver últimos resultados"
    putStrLn "3. Salir"
    putStrLn "Selecciona una opción (1, 2 o 3):"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Iniciando nuevo juego..."
            iniciarJuego estado
        "2" -> do
            nuevoEstado <- mostrarUltimosResultados estado
            menu nuevoEstado  -- Continuamos con el mismo estado
        "3" -> do
            putStrLn "¡Gracias por jugar!"
            exitSuccess
        _ -> do
            putStrLn "Opción inválida, intente nuevamente."
            menu estado

-- Función principal
main :: IO ()
main = do
    -- Estado inicial vacío
    let estadoInicial = [] :: Estado
    _ <- menu estadoInicial
    return ()

-- Función auxiliar para Maybe con <|>
orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing y = y
