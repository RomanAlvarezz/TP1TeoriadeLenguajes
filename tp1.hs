import System.IO
import Data.Char (isDigit)

sgetLine = do hSetEcho stdin False
              palabra <- sgetLine' 0
              hSetEcho stdin True
              return palabra

sgetLine' contador = do x <- getChar
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

ingresarSecuencia = do secJugador1 <- ingresoJugada 1
                       secJugador2 <- ingresoJugada 2
                       let secuenciaZip = zip secJugador1 secJugador2
                       putStrLn("Jugadas realizadas:")
                       print secuenciaZip  
                       return (jugarPartida secuenciaZip  0 0)


jugarPartida :: [(Char, Char)] -> Int -> Int -> (Int, Int)
jugarPartida [] puntos1 puntos2 = (puntos1, puntos2)
jugarPartida ((x, y):xs) puntos1 puntos2 
              | x == 'R' && y == 'R' || x == 'P' && y == 'P' || x == 'S' && y == 'S' = jugarPartida xs puntos1 puntos2
              | x == 'R' && y == 'S' || x == 'P' && y == 'R' || x == 'S' && y == 'P' = jugarPartida xs (puntos1+1) puntos2
              | x == 'S' && y == 'R' || x == 'R' && y == 'P' || x == 'P' && y == 'S' = jugarPartida xs puntos1 (puntos2+1)

secuenciarPartidas 0  c1 c2 pActual 
                        |(c1 > c2)  = putStrLn ("Juego finalizado\nGano el jugador 1 \nPuntos totales Jugador 1: " ++ show c1 ++ "\nPuntos totales Jugador 2: " ++ show c2)
                        |(c1 < c2)  = putStrLn ("Juego finalizado\nGano el jugador 2 \nPuntos totales Jugador 1: " ++ show c1 ++ "\nPuntos totales Jugador 2: " ++ show c2)
                        |(c1 == c2) = putStrLn ("Juego finalizado\nEmpate \nPuntos totales Jugador 1: " ++ show c1 ++ "\nPuntos totales Jugador 2: " ++ show c2)

secuenciarPartidas cp c1 c2 pActual = do (puntos1, puntos2) <- ingresarSecuencia
                                         putStrLn ("Resultado de la partida " ++ show pActual ++ ":")
                                         putStrLn ("Puntos Jugador 1: " ++ show puntos1)
                                         putStrLn ("Puntos Jugador 2: " ++ show puntos2)
                                         putStrLn ("-------------------------------------------")
                                         secuenciarPartidas (cp-1) (c1 + puntos1) (c2 + puntos2) (pActual+1)

ingresoCantPartidas = do putStrLn "Ingrese la cantidad de partidas a jugar"
                         cantPartidas <- getLine   
                         if cantPartidas /= [] && auxDigit cantPartidas     
                             then return (read cantPartidas :: Int)                          
                             else do
                                 putStrLn "Por favor ingrese un número válido."
                                 ingresoCantPartidas                            

ingresoJugada numJugador= do putStrLn ("Jugador " ++ show numJugador ++ " ingrese la secuencia")
                             secJugador <- sgetLine
                             if secJugador /= [] 
                                then return secJugador
                                else do putStrLn "Por favor ingrese una secuencia válida."
                                        ingresoJugada numJugador

auxDigit [] = True
auxDigit (x:xs) = if isDigit x then auxDigit xs else False


juego = do cantPartidas <- ingresoCantPartidas
           secuenciarPartidas cantPartidas 0 0 1

main = juego

