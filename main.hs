import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)


printearBordeVertical :: Int -> IO()
printearBordeVertical 1 = putStr "-----\n"
printearBordeVertical n = do
                        putStr "-----" 
                        printearBordeVertical (n-1)


type Fila = [Char]
type Tablero = [Fila]

printearTablero :: Int -> Tablero -> IO()
printearTablero n  [] = putStrLn ""
printearTablero n  (x:xs) = do 
                                printearBordeVertical n
                                printearFila n x
                                printearBordeVertical n
                                printearTablero n xs 


printearFila :: Int -> Fila -> IO()
printearFila 1 (x:_) = do
                        putStr "| "
                        putChar x
                        putStr " |\n"
printearFila n (x:xs) = do 
                        putStr "| "
                        putChar x
                        putStr " |"
                        printearFila (n-1) xs

test :: IO()
test = do
    tablero <- construirTablero
    printearTablero (length (head tablero)) tablero


construirTablero :: IO Tablero
construirTablero = do
    putStrLn "Ingresa las medidas del tablero:"
    putStr "Filas: "
    n <- getLine
    putStr "Columnas: "
    m <- getLine
    return (construirTableroVacio (read n) (read m))

construirTableroVacio :: Int -> Int -> Tablero
construirTableroVacio _ 0 = []
construirTableroVacio n m = (construirFilaVacia n) : construirTableroVacio n (m-1)

construirFilaVacia :: Int -> Fila
construirFilaVacia 0 = []
construirFilaVacia n = ' ' : construirFilaVacia (n-1) 

juegoLoop :: Tablero -> Char -> IO ()
juegoLoop tablero jugador = do
    let columnas = length (head tablero)
    putStrLn $ "\nTurno del jugador: " ++ [jugador]
    
    nuevoTablero <- jugarTurno tablero jugador
    
    let siguienteJugador = if jugador == 'X' then 'O' else 'X'
    juegoLoop nuevoTablero siguienteJugador

jugarTurno :: Tablero -> Char -> IO Tablero
jugarTurno tablero jugador = do
    let columnas = length (head tablero)
    printearTablero columnas tablero
    putStr $ "Jugador " ++ [jugador] ++ ", ingresa el número de columna: "
    colStr <- getLine
    let col = read colStr
    
    case dropFicha tablero col jugador of
        Just nuevoTablero -> return nuevoTablero
        Nothing -> do
            putStrLn "Columna inválida o llena! Intenta otra."
            jugarTurno tablero jugador


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

-- Iniciar juego completo
iniciarJuego :: IO ()
iniciarJuego = do
    tablero <- construirTablero
    juegoLoop tablero 'X'  -- Empieza el jugador X

-- Test del juego completo
testJuegoCompleto :: IO ()
testJuegoCompleto = do
    let tablero = construirTableroVacio 4 6
    juegoLoop tablero 'X'