import System.IO
import Data.Char (intToDigit)
import Data.Char (digitToInt)
import Data.Char (isDigit)

sgetLine = do 
    hSetEcho stdin False
    palabra <- sgetLine' 0
    hSetEcho stdin True
    return palabra

sgetLine' contador = do 
    x <- getChar
    if x == '\n' 
        then do 
            hSetEcho stdin True
            putStr ("Contador: " ++ show contador ++ "\n")                                   
            hSetEcho stdin False
            return []
        else if x == 'R' || x == 'P' || x == 'S'
            then do 
                xs <- sgetLine' (contador + 1)                 
                return (x:xs)
            else do 
                xs <- sgetLine' contador                       
                return xs

ingresarSecuencia =      do putStrLn "Jugador 1 ingrese la secuencia"
                            secJugador1 <- sgetLine
                            putStrLn "Jugador 2 ingrese la secuencia"
                            secJugador2 <- sgetLine
                            --putStrLn secJugador1
                            --putStrLn secJugador2
                            let secuenciaZip = zip secJugador1 secJugador2
                            --print secuenciaZip  
                            return (jugarPartida secuenciaZip  0 0)


jugarPartida :: [(Char, Char)] -> Int -> Int -> (Int, Int)
jugarPartida [] puntos1 puntos2 = (puntos1, puntos2)
jugarPartida ((x, y):xs) puntos1 puntos2 
              | x == 'R' && y == 'R' || x == 'P' && y == 'P' || x == 'S' && y == 'S' = jugarPartida xs puntos1 puntos2
              | x == 'R' && y == 'S' || x == 'P' && y == 'R' || x == 'S' && y == 'P' = jugarPartida xs (puntos1+1) puntos2
              | x == 'S' && y == 'R' || x == 'R' && y == 'P' || x == 'P' && y == 'S' = jugarPartida xs puntos1 (puntos2+1)

secuenciarPartidas 0  c1 c2 
                        |(c1 > c2)  = putStrLn ("Gano el jugador 1 \n Puntos Jugador 1: " ++ show c1 ++ "\n Puntos Jugador 2: " ++ show c2)
                        |(c1 < c2)  = putStrLn ("Gano el jugador 2 \n Puntos Jugador 1: " ++ show c1 ++ "\n Puntos Jugador 2: " ++ show c2)
                        |(c1 == c2) = putStrLn ("Empate | Puntos Jugador 1: " ++ show c1 ++ " Puntos Jugador 2: " ++ show c2)
secuenciarPartidas cp c1 c2 = do (puntos1, puntos2) <- ingresarSecuencia
                                 putStrLn ("Puntos Jugador 1: " ++ show puntos1)
                                 putStrLn ("Puntos Jugador 2: " ++ show puntos2)
                                 secuenciarPartidas (cp-1) (c1 + puntos1) (c2 + puntos2)

ingresoCantPartidas = do
    putStrLn "Ingrese la cantidad de partidas a jugar"
    cantPartidas <- getLine
    if all isDigit cantPartidas && not (null cantPartidas)  -- Verifica si toda la cadena son dígitos
        then return (read cantPartidas :: Int)              -- Convierte la cadena en un número entero
        else do
            putStrLn "Por favor ingrese un número válido."
            ingresoCantPartidas                             -- Llama de nuevo si la entrada no es válida

juego = do cantPartidas <- ingresoCantPartidas
           secuenciarPartidas cantPartidas 0 0

main = juego


