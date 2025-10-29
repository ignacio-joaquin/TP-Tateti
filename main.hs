main :: IO ()
main = do

    
    putStrLn "=== 3 IN A LINE GAME ==="
    putStrLn "Rules:"
    putStrLn "- Players take turns placing X and O"
    putStrLn "- Each player has exactly 3 pieces"
    putStrLn "- Win by getting 3 in a row, column, or diagonal"
    putStrLn ""



-- printearTablero :: IO()
-- printearTablero = do
--                     printearBordeVertical 8
--                     printearFila 8 ['x', ' ', 'o', ' ', ' ', 'o','o','x']
--                     printearBordeVertical 8

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
test = printearTablero 8 [['x', ' ', 'o', ' ', ' ', 'o','o','x'],['x', ' ', 'o', ' ', ' ', 'o','o','x'],['x', ' ', 'o', ' ', ' ', 'o','o','x'],['x', ' ', 'o', ' ', ' ', 'o','o','x']]