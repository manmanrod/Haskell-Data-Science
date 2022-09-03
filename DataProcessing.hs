module DataProcessing
( countGames
, countGamesFromPublisher
, countGamesFromDeveloper
, mostExpensiveGame
, cheapestGame
, nBestRatedGames
, nMostPlayedGames
, nameMostRatedGames
, valuesMostRatedGames
, nMostPlayedCategories
, nMostPlayedCategoriesR
, avgOwnersMostExpensiveGame
, genreCounter
, genreOrdered
, countPlatforms
, nGamesInEnglish
, nMostCommonTags
, nWithAchievements
, nGamesReleasedBetween
, nGamesAboveAge
, findGame
, listGamesReleasedBetween
) where 

-- ---------------------------------------------------------
-- Modulo que contine las funciones de procesado de datos
-- ---------------------------------------------------------

-- Imports
import Data.List
import qualified Data.Map as M
import qualified Data.Map.Strict as Strict
import Data.Dates
import PilaTA

import Datos
import Graficos



-- ---------------------------------------------------------------------
-- Funciones                                                          --
-- ---------------------------------------------------------------------



-- ---------------------------------------------------------------------
-- Devuelve el número de juegos que hay cargados en el archivo csv
-- ---------------------------------------------------------------------

countGames :: [Game] -> Int
countGames xs = length xs


-- ---------------------------------------------------------------------
-- Devuelve el número de juegos que corresponde al editor p
-- ---------------------------------------------------------------------

countGamesFromPublisher :: String -> [Game] -> Int
countGamesFromPublisher p [] = 0
countGamesFromPublisher p (x:xs)
    | elem p pub = 1 + countGamesFromPublisher p xs
    | otherwise = countGamesFromPublisher p xs
    where pub = getPublisher x


-- ---------------------------------------------------------------------
-- Devuelve el número de juegos que corresponde al desarrollador d
-- ---------------------------------------------------------------------

countGamesFromDeveloper :: String -> [Game] -> Int
countGamesFromDeveloper d xs = length [1 | x <- xs, elem d (getDeveloper x)]


-- ---------------------------------------------------------------------
-- Devuelve el juego más caro
-- ---------------------------------------------------------------------

mostExpensiveGame :: [Game] -> Game
mostExpensiveGame = foldr1 (\x acc -> if (getPrice x) > (getPrice acc) then x else acc)


-- ---------------------------------------------------------------------
-- Devuelve el número de juegos más barato
-- ---------------------------------------------------------------------

cheapestGame :: [Game] -> Game
cheapestGame gs = foldl (\acc x -> if (getPrice x) < (getPrice acc) then x else acc) (gs!!0) gs


-- ---------------------------------------------------------------------
-- Devuelve la lista formada de los pares (nombre, valoraciones positivas)
--  de los n juegos mejor valorados en orden decreciente
-- ---------------------------------------------------------------------

nBestRatedGames :: Int -> [Game] -> [(String, Int)]
nBestRatedGames n gs = take n $ reverse $ sortBy (\(_,a) (_,b) -> compare a b) [ (getName g, getPosRatings g) | g<-gs ]


-- ---------------------------------------------------------------------
-- Función derivada. Devuelve el nombre de los n juegos mejor valorados
-- ---------------------------------------------------------------------

nameMostRatedGames :: Int -> [Game] -> [String]
nameMostRatedGames n gs = map (\x -> fst x) (nBestRatedGames n gs)


-- ---------------------------------------------------------------------
-- Función derivada. Devuelve las valoraciones positivas de los n juegos
--   mejor valorados
-- ---------------------------------------------------------------------

valuesMostRatedGames :: Int -> [Game] -> [Int]
valuesMostRatedGames n gs = map (\x -> snd x) (nBestRatedGames n gs)


-- ---------------------------------------------------------------------
-- Devuelve la lista formada de los pares (nombre, tiempo medio jugado)
--   de los n juegos más jugados en orden decreciente
-- ---------------------------------------------------------------------

nMostPlayedGames :: Int -> [Game] -> [(String,Int)]
nMostPlayedGames n gs = take n $ reverse $ sortBy (\(_,a) (_,b) -> compare a b )  [ (getName g, getAvgPlaytime g) | g<-gs ]


-- ---------------------------------------------------------------------
-- Devuelve la lista formada de los pares (categorías, tiempo medio jugado)
--   de los n juegos más jugados en orden decreciente
-- ---------------------------------------------------------------------

nMostPlayedCategories :: Int -> [Game] -> [([String],Int)]
nMostPlayedCategories n gs = take n $ reverse $ sortBy (\(_,a) (_,b) -> compare a b )   [ (getCategories g, getAvgPlaytime g) | g<-gs ]


nMostPlayedCategoriesR :: Int -> [Game] -> [String]
nMostPlayedCategoriesR n gs = nMostPlayedCategoriesRAux n gs []

nMostPlayedCategoriesRAux n [] xs = take n $ nub xs --nub para no repetir elementos
nMostPlayedCategoriesRAux n (g:gs) xs = nMostPlayedCategoriesRAux n gs (xs++(getCategories g))


-- ---------------------------------------------------------------------
-- Devuelve la media de jugadores del juego más caro
-- ---------------------------------------------------------------------

avgOwnersMostExpensiveGame :: Game -> Int
avgOwnersMostExpensiveGame g = div ((lBound x) + (uBound x)) 2
    where x = getOwners g


-- ---------------------------------------------------------------------
-- Devuelve la lista formada de los pares 
--  (categoría, veces que aparece dicha categoría) de todos los juegos
--   ordenados en forma decreciente
-- ---------------------------------------------------------------------

genreCounter :: [Game] -> [(String,Int)]
genreCounter gs = M.assocs (buildMapCounter M.empty (concat [getGenres g | g <- gs]))

buildMapCounter :: M.Map String Int -> [String] -> M.Map String Int
buildMapCounter m [] = m
buildMapCounter m (s:ss) = buildMapCounter (introdMap m s) ss 
    where introdMap d k = if M.member k d then Strict.insertWith (+) k 1 d else M.insert k 1 d

genreOrdered :: [Game] -> [(String,Int)]
genreOrdered gs = reverse $ sortBy (\(_,a) (_,b) -> compare a b ) (genreCounter gs)


-- ---------------------------------------------------------------------
-- Devuelve la tupla de enteros con el número de juegos que estan 
--  disponibles en (Windows, Mac, Linux)
-- ---------------------------------------------------------------------

countPlatforms :: [Game] -> (Int,Int,Int) -- (Windows,Mac,Linux)
countPlatforms gs = cuentaConPilas vacia vacia vacia (concat [ getPlatforms g | g <- gs])

cuentaConPilas :: Pila String -> Pila String -> Pila String -> [String] -> (Int,Int,Int)
cuentaConPilas w m l [] = (counterPila w 0, counterPila m 0, counterPila l 0)
cuentaConPilas w m l (s:ss)
    | s == "windows"  = cuentaConPilas (apila s w) m l ss
    | s == "mac"      = cuentaConPilas w (apila s m) l ss
    | otherwise       = cuentaConPilas w m (apila s l) ss

counterPila :: Pila String -> Int -> Int
counterPila p c
    | esVacia p  = c
    | otherwise  = counterPila (desapila p) (c+1)


-- ---------------------------------------------------------------------
-- Devuelve el número de juegos disponibles en Inglés
-- ---------------------------------------------------------------------

nGamesInEnglish :: [Game] -> Int
nGamesInEnglish [] = 0
nGamesInEnglish (g:gs) = case inEnglish g of
    True -> 1 + nGamesInEnglish gs
    False -> nGamesInEnglish gs


-- ---------------------------------------------------------------------
-- Devuelve la lista formada de los pares 
--  (etiqueta, veces que aparece dicha etiqueta) con las n etiquetas
--   más usadas en orden decreciente
-- --------------------------------------------------------------------

nMostCommonTags :: [Game] -> Int -> [(String,Int)]
nMostCommonTags gs n = take n $ reverse $ sortBy (\(_,a) (_,b) -> compare a b) d
    where d = M.assocs (buildMapCounter M.empty (concat [getTags g | g <- gs]))


-- ---------------------------------------------------------------------
-- Devuelve el número de juegos que tienen logros de todos los juegos
-- --------------------------------------------------------------------

nWithAchievements :: [Game] -> Int
nWithAchievements [] = 0
nWithAchievements (g:gs)
    | getAchievements g > 0  = 1 + nWithAchievements gs
    | otherwise              = nWithAchievements gs


-- ---------------------------------------------------------------------
-- Devuelve el número de juegos lanzados entre las fechas x e y
-- ---------------------------------------------------------------------

nGamesReleasedBetween :: String -> String -> [Game] -> Int
nGamesReleasedBetween x y gs = length [ 1 | g <- gs, let d = getRelDate g, isAfter d xp && isBefore d yp]
    where xp = dateParser x
          yp = dateParser y

isBefore :: DateTime -> DateTime -> Bool
isBefore x y
    | year x < year y = True
    | year x > year y = False
    | otherwise       = if month x < month y then True
                            else if month x > month y then False 
                                    else if day y <= day y then True else False

isAfter :: DateTime -> DateTime -> Bool
isAfter x y
    | year x > year y = True
    | year x < year y = False
    | otherwise       = if month x > month y then True
                            else if month x < month y then False 
                                    else if day y >= day y then True else False


-- ---------------------------------------------------------------------
-- Devuelve el número de juegos para mayores de x edad
-- ---------------------------------------------------------------------

gamesAboveAge :: Int -> [Game] -> [Game]
gamesAboveAge x gs = [ g | g <- gs, getMinAge g >= x]

nGamesAboveAge :: Int -> [Game] -> Int
nGamesAboveAge x gs = length (gamesAboveAge x gs)


-- ---------------------------------------------------------------------
-- Devuelve el juego cuyo nombre coincide con el parámetro nam
-- --------------------------------------------------------------------

findGame :: String -> [Game] -> Game
findGame nam gs = head [g | g <- gs, let n = getName g, n == nam]


-- ---------------------------------------------------------------------
-- Devuelve los juegos lanzados entre las fechas dadas
-- ---------------------------------------------------------------------

listGamesReleasedBetween :: String -> String -> [Game] -> [Game]
listGamesReleasedBetween x y gs = [ g | g <- gs, let d = getRelDate g, isAfter d xp && isBefore d yp]
    where xp = dateParser x
          yp = dateParser y
