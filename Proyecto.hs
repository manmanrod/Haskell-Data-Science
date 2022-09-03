-- --------------------------------------------------
-- --------  Archivo con la funcion Main  -----------
-- --------------------------------------------------

-- Imports
import System.Environment
import System.Directory
import Control.Exception 
import Text.CSV


import Datos
import DataProcessing
import Graficos


-- ---------------------------------------------------------------------
-- Funciones de procesado de datos del CSV
-- ---------------------------------------------------------------------

-- Control de Errores
controlarError err = print "Error leyendo archivo"


-- Lectura y procesado
procesarCSV csv = do
    let datos = tail csv -- omitir linea con titulos
    let juegos = parseGameList datos -- alojar los datos en la variable "juegos"
    
    putStrLn ("========== DATOS OBTENIDOS ========== ")
    putStrLn ("\nVamos a analizar un total de " ++ show (countGames juegos) ++ " juegos")

    let pub = "Activision"
    putStrLn ("\nEl numero de juegos publicados por " ++ pub ++ " es " ++ show (countGamesFromPublisher pub juegos))

    let dev = "Valve"
    putStrLn ("\nEl numero de juegos desarrollados por " ++ dev ++ " es " ++ show (countGamesFromDeveloper dev juegos))


    let juegoMasCaro = mostExpensiveGame juegos
    putStrLn ("\nEl juego mas caro es " ++ show (getName juegoMasCaro) ++ " desarrollado por "
        ++ show ( head (getDeveloper juegoMasCaro) ) ++ " con un precio total de $" ++ show (getPrice juegoMasCaro)  )


    let juegoMasBarato = cheapestGame juegos
    putStrLn ("\nEl juego mas barato es " ++ show (getName juegoMasBarato) ++ " desarrollado por "
        ++ show ( head (getDeveloper juegoMasBarato) ) ++ " con un precio total de $" ++ show (getPrice juegoMasBarato)  )


    let nJuegosMejorValorados = nBestRatedGames 10 juegos
    putStrLn ("\nLos 10 juegos mejor valorados son: ")
    sequence_ [putStrLn $ show x | x <- nJuegosMejorValorados]


    let nJuegosMasJugados = nMostPlayedGames 10 juegos
    putStrLn ("\nLos 10 juegos más jugados son: ")
    sequence_ [putStrLn $ show x | x <- nJuegosMasJugados]
    

    let nCategoriasMasJugadasRecursivo = nMostPlayedCategoriesR 3 juegos
    putStrLn ("\n(Recursivo) Las 3 categorías más jugadas son: " ++ show (nCategoriasMasJugadasRecursivo) )
    

    let propietariosJuegoMasCaro = avgOwnersMostExpensiveGame juegoMasCaro
    putStrLn ("\nPropietarios medio del juego mas caro: " ++ show (propietariosJuegoMasCaro) )


    let contadorGeneros = genreCounter juegos
    putStrLn "\nEl numero de juegos por género es:"
    sequence_ [putStrLn $ show t | t <- contadorGeneros]


    imprimirPlataformas juegos


    let juegosEnIngles = nGamesInEnglish juegos
    putStrLn ("\nHay un total de " ++ show juegosEnIngles ++ " juegos en Inglés." ++
        "\nHay " ++ show (countGames juegos - juegosEnIngles) ++ " que no soportan el Inglés")

    
    let masTags = nMostCommonTags juegos 10
    putStrLn ("\nLas 10 tags que mas se repiten son: ")
    sequence_ [putStrLn (show t) | t <- masTags]


    let nLogros = nWithAchievements juegos
    putStrLn ("\nTienen un sistema de logros " ++ show nLogros ++ " juegos. Un " ++ 
        show (((fromIntegral nLogros)/(fromIntegral (countGames juegos)))*100) ++ "% de los juegos.")

    
    let d1 = "2002-03-04"
    let d2 = "2010-05-12"
    putStrLn ("\nEntre el " ++ d1 ++ " y el " ++ d2 ++ " se lanzaron " ++ 
        show (nGamesReleasedBetween d1 d2 juegos) ++ " juegos.")


    let age = 18
    putStrLn ("\nEl numero de juegos para mayores de " ++ show age ++ " es: " ++ 
        show (nGamesAboveAge age juegos) ++ ".")


    putStrLn ("\n========== GRÁFICOS ========== ")

    mostrarGraficos juegos

    -- fin procesarCSV


-- Funciones auxiliares de procesado de datos
imprimirPlataformas :: [Game] -> IO()
imprimirPlataformas gs = do
    let (w,m,l) = countPlatforms gs
    putStrLn ("\nEl numero de juegos disponibles en cada plataforma es: \nWindows: " 
        ++ (show w) ++ "\nMac: " ++ (show m) ++ "\nLinux: " ++ (show l))


-- Funcion para generar los graficos
mostrarGraficos :: [Game] -> IO()
mostrarGraficos juegos = do
    let nombreJuegosMejorValorados = nameMostRatedGames 10 juegos
    let valoresJuegosMejorValorados = map (\x -> div x 100000) $ valuesMostRatedGames 10 juegos
    putStrLn ("\nGráfico circular - Los 10 juegos mejor valorados")
    print(graficoCircular "Los 10 juegos mejor valorados" valoresJuegosMejorValorados nombreJuegosMejorValorados  )


    let contadorGeneros = genreOrdered juegos
    let nombreGeneros = [ n | (n,v) <- contadorGeneros]
    let valoresGeneros = [ v | (n,v) <- contadorGeneros]
    putStrLn ("\nGráfico de barras - Generos a los que pertenecen los juegos")
    print (graficoBarras "Generos" valoresGeneros nombreGeneros)


    let d1 = "2002-03-04"
    let d2 = "2010-05-12"
    let preciosJuegos = map (\x ->  realToFrac (getPrice x) * 10 ) $ take 50 $ listGamesReleasedBetween d1 d2 juegos
    let valoresJuegos = zip [0..] preciosJuegos

    putStrLn ("\nGráfico de ejes - Evolución de los precios de 50 juegos entre " ++ d1 ++ " y " ++ d2)
    print ( graficoEjes ("Evolucion de precios entre " ++ d1 ++ " y " ++ d2) valoresJuegos d1 d2 )





-- ---------------------------------------------------------------------
-- Funciones principales
-- ---------------------------------------------------------------------

-- Funcion de lecura de archivo CSV
leerCSV :: String -> IO ()
leerCSV rutaArchivo = do
  contenido <- readFile rutaArchivo
  let csv = parseCSV rutaArchivo contenido
  either controlarError procesarCSV csv


-- Funcion main para compilar
main :: IO()
main = do
    args <- getArgs
    case length args of
        1 -> leerCSV (head args)
        _ -> putStrLn "Error. El formato es procesarCSV [ruta de archivo]"

